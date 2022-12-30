Simple model to simulate the BPF program which is a part of runqslower.

Run it with:

    nix-shell -p "ghc.withPackages (
        pkgs:
            with pkgs;
            [ heaps
              statistics
              containers
              random-fu
              random-source
              vector
              system ])"

One time unit is 1ns.

> {-# LANGUAGE OverloadedStrings #-}
> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE RecordWildCards #-}
> {-# LANGUAGE NamedFieldPuns #-}

Bug workaround for the stats package.

> {-# LANGUAGE GADTs #-}
>
> import Control.Monad
> import Control.Monad.Trans.State
> import Control.Monad.IO.Class
> import Data.Heap (Heap (..), Entry (..), union)
> import qualified Data.Heap as H hiding (minimum)
> import Data.Sequence (Seq (..))
> import qualified Data.Sequence as S
>
> import System.Random.MWC
> import qualified Data.Random as Fu
> import Data.Random.Source.MWC

In this model we use only normally distributed variables, but it's easy to experiment with more type of distribution. Include those for such experiments:

    import Data.Random.Distribution.Poisson
    import Data.Random.Distribution.Exponential

> import Data.Random.Distribution.Normal
>
> import System.Environment
>
> import Debug.Trace (trace, traceIO)

Convenient type aliases.

To which CPU the event is assigned:

> type Cpu = Int

For how long the process has to be running.

> type Clock = Int

The current latency for one ProcessRun event.

> type RunLatency = Int

Event is a data type we're going to use to simulate transitions of the state
machine.

> data Event = ProcessStart Clock
>            | ProcessQueue RunLatency Clock Cpu
>            | BpfQueue Cpu
>            | BpfReport Cpu
>            | ProcessRun Clock Cpu
>            | ProcessDequeue Clock Cpu
>            | ProcessFinish Clock Cpu
>            | BpfPerfOutput RunLatency Cpu
>            | NetScraping deriving (Show)

Heap of events, for performance reasons. The first field, priority, is equal to
the point in time when the event was created in the system.

> type Events = Heap (Entry Int Event)
>
> data SimulationState = SimulationState { stateEvents        :: Events
>                                        , stateCpuQueues     :: Seq Int
>                                        , bpfLatency         :: Int
>                                        , runLatency         :: Int
>                                        , threshold          :: Int
>                                        , mapSize            :: Int
>                                        , stateGen           :: Gen RealWorld
>                                        }
> instance Show SimulationState where
>   show (SimulationState events cpus bpfLat runLat threshold mapSize gen) =
>     "Events: " ++ show events ++ "\n" ++
>     "CPU: " ++ show cpus ++ "\n" ++
>     "Latency threshold: " ++ show threshold ++ "\n" ++
>     "Map size: " ++ show mapSize ++ "\n" ++
>     "Bpf Lat: " ++ show bpfLat ++ "\n" ++
>     "Run Lat: " ++ show runLat ++ "\n" ++
>     "Overhead: " ++ show (bpfLat ./. runLat) ++ "\n"

Initial lifetime of a new process, short lived processes. Fiddling with this
and the number of processes can produce systems with different position in the
phase space, e.g. there will be not enough BPF load, so the overhead will be
always converging to zero; or there will be too much load so everything will
explode; or both BPF load and process execution will balance each other
producing some stable solution.

> processLifetime = 100000

Upper boundary for time in the simulation.

> eventTimeLimit = 10000000000

Upper boundary for how many runs of simulations are going to be performed.

> simulationLoopsLimit = 10000

Sequence reprecenting CPUs load in the system. All CPUs start idle.

> cpus = S.fromList $ take 8 $ repeat 0

As an argument we accept the time boundary for simulation in loop rounds. The
initial conditions are simply bunch of processes starting together. If we, for
example, would like to simulate randomly spawned processes, we could specify
Poisson distributed intervals:

     intervals <- replicateM 1000 $ do
         -- rate lambda = 1 / mu
         let mu = n :: Double
         time <- (Fu.runRVar (exponential mu) gen) :: IO Double
         return $ round time

     let arrivals = scanl1 (+) intervals

There is one argument, number of initial processes to simulate. The initial
state is a bunch of processes starting together.

> main = do
>     processNum <- getDoubleArg
>     gen <- createSystemRandom
>
>     let arrivals = take (round processNum) $ repeat 0
>     let events = map createProcess arrivals
>     let loopFunc = mainLoop 1 eventTimeLimit simulationLoopsLimit
>     let initState = SimulationState { stateEvents = H.fromList events
>                                     , stateCpuQueues = cpus
>                                     , bpfLatency = 1
>                                     , runLatency = 1
>                                     , threshold = 1000
>                                     , mapSize = 0
>                                     , stateGen = gen }
>     state <- execStateT loopFunc initState
>     showOverhead state
>  where
>    createProcess arrivalTime = Entry arrivalTime $ ProcessStart processLifetime

The main loop of the simulation.

> mainLoop counter limit counterLimit = trace ("Step " ++ (show $ counter)) $ do
>     state@SimulationState{stateEvents} <- get
>     case (H.viewMin $ stateEvents) of
>         Just (entry, newEvents) -> trace ("Process " ++ (show entry)) $ do
>             if (priority entry < limit && counter < counterLimit) then do
>                 put state { stateEvents = newEvents }
>                 newState <- processEvent entry
>                 trace ("New state " ++ (show newState)) $ put newState
>                 mainLoop (counter + 1) limit counterLimit
>             else return ()
>         Nothing -> return ()

Every event processing function takes the event and the current simulation
state as the input, then modifies the state by inserting more depending events.
The following section contains such transformation functions.

Start represents the start of process, so it happens only once per life time.
It triggers two events, enqueuing of the projcess into the runqueue and
recording it in runqslower. They're not independent, but concatenated in time
as everything happens within the same process.

> processEvent (Entry time (ProcessStart clock)) = do
>     state@SimulationState{ stateCpuQueues
>                          , stateEvents
>                          , bpfLatency
>                          , runLatency
>                          , threshold
>                          , mapSize
>                          , stateGen} <- get

Assign this process and all the following events to a least loaded CPU.

>     let leastLoaded = minimum stateCpuQueues
>     let (Just assignedCpu) = S.elemIndexL leastLoaded stateCpuQueues
>
>     let bpfStartEvent = BpfQueue assignedCpu
>     let bpfFinishEvent = BpfReport assignedCpu
>     let processEvent = ProcessQueue 0 clock assignedCpu
>
>     bpfStartLat <- liftIO $ latency bpfStartEvent state
>     bpfFinishLat <- liftIO $ latency bpfFinishEvent state
>     runLat <- liftIO $ latency processEvent state
>
>     let newBpfStartEvent = Entry (time + bpfStartLat) bpfStartEvent
>     let newBpfFinishEvent = Entry (time + runLat + bpfFinishLat) bpfFinishEvent
>     let newProcessEvent = Entry (time + runLat) (ProcessQueue runLat clock assignedCpu)
>
>     let newEvents = union (H.fromList [newBpfStartEvent, newProcessEvent, newBpfFinishEvent]) stateEvents
>
>     let updatedCpus = S.adjust' (+ 1) assignedCpu stateCpuQueues
>     let newBpfLatency = bpfLatency + bpfStartLat + bpfFinishLat
>     let newRunLatency = runLatency + runLat
>     trace ("New events " ++ show [newBpfStartEvent, newProcessEvent, newBpfFinishEvent]) $
>         trace ("BpfStartLat, BpfFinishLat, RunLat " ++ show [bpfStartLat, bpfFinishLat, runLat]) $
>         return $ state { stateEvents = newEvents
>                        , stateCpuQueues = updatedCpus
>                        , bpfLatency = newBpfLatency
>                        , runLatency = newRunLatency }

Similarly to start, enqueuing of the projcess into the runqueue and recording
it in runqslower. This even happens multiple times per process life time.

> processEvent (Entry time (ProcessQueue queueLat clock cpu)) = do
>     state@SimulationState{ stateEvents
>                          , bpfLatency
>                          , runLatency
>                          , threshold } <- get
>     let bpfStartEvent = BpfQueue cpu
>     let bpfFinishEvent = BpfReport cpu
>     let bpfPerfOutputEvent = BpfPerfOutput queueLat cpu
>     let processEvent = ProcessRun clock cpu
>
>     bpfStartLat <- liftIO $ latency bpfStartEvent state
>     bpfFinishLat <- liftIO $ latency bpfFinishEvent state
>     bpfPerfOutputLat <- liftIO $ latency bpfPerfOutputEvent state
>     runLat <- liftIO $ latency processEvent state
>
>     let bpfReportLat = if queueLat > threshold
>         then bpfFinishLat + bpfPerfOutputLat
>         else bpfFinishLat
>
>     let newBpfStartEvent = Entry (time + bpfStartLat) bpfStartEvent

Remember that the priority value (time + queueLat + bpfReportLat) is the new
position of the event on the timeline, it doesn't mean run latency will be
included into the overhead.

>     let newBpfFinishEvent = Entry (time + queueLat + bpfReportLat) bpfFinishEvent
>     let newProcessEvent = Entry (time + runLat) $ decrementClock processEvent (runLat - (bpfStartLat + bpfReportLat))
>
>     let newEvents = union (H.fromList [newBpfStartEvent, newProcessEvent, newBpfFinishEvent]) stateEvents
>
>     let newBpfLatency = bpfLatency + bpfStartLat + bpfReportLat
>     let newRunLatency = runLatency + runLat
>     liftIO $ traceIO ("New events " ++ show [newBpfStartEvent, newProcessEvent, newBpfFinishEvent])
>     liftIO $ traceIO ("BpfStartLat, BpfReportLat, RunLat " ++ show [bpfStartLat, bpfReportLat, runLat])
>
>     return $ state { stateEvents = newEvents
>                    , bpfLatency = newBpfLatency
>                    , runLatency = newRunLatency }

The BPF program has recorded start of the time, spent in the queue.

> processEvent (Entry time (BpfQueue cpu)) = do
>     state@SimulationState{ mapSize } <- get
>     return $ state { mapSize = mapSize + 1 }

The BPF program has recorded the end of queuing time and report it.

> processEvent (Entry time (BpfReport cpu)) = do
>     state@SimulationState{ mapSize } <- get

The situation when a not-yet-recorded element has to be removed from the map
could be considered as a record miss. Out-of-order record/report events will
inflate the map.

>     return $ if mapSize > 0
>       then state { mapSize = mapSize - 1 }
>       else state

The task leaves the queue.

> processEvent (Entry time (ProcessDequeue clock cpu)) = do
>     state@SimulationState{ stateCpuQueues
>                          , stateEvents
>                          , runLatency } <- get

     The process is already pinned to the CPU, although we may consider
     rebalancing

     let leastLoaded = minimum stateCpuQueues
     let (Just assignedCpu) = S.elemIndexL leastLoaded stateCpuQueues

>     let processEvent = ProcessQueue 0 clock cpu
>
>     runLat <- liftIO $ latency processEvent state
>
>     let newProcessEvent = Entry (time + runLat) (ProcessQueue runLat clock cpu)
>     let updatedCpus = S.adjust' (+ 1) cpu stateCpuQueues
>     let newRunLatency = runLatency + runLat
>
>     let newEvents = union (H.fromList [newProcessEvent]) stateEvents
>
>     liftIO $ traceIO ("New events " ++ show [newProcessEvent])
>     return $ state { stateEvents = newEvents
>                    , stateCpuQueues = updatedCpus
>                    , runLatency = newRunLatency }

The task is about to be run, but has no time on the clock. Finish

> processEvent (Entry time (ProcessRun 0 cpu)) = do
>     state@SimulationState{ stateCpuQueues } <- get
>     let updatedCpus = S.adjust' (\c -> c - 1) cpu stateCpuQueues
>     return $ state { stateCpuQueues = updatedCpus }

The task is doing some work with more time on clock.

> processEvent (Entry time (ProcessRun clock cpu)) = do
>     state@SimulationState{ stateCpuQueues
>                          , stateEvents
>                          , runLatency } <- get
>     let processEvent = ProcessDequeue clock cpu
>     runLat <- liftIO $ latency processEvent state
>
>     let newProcessEvent = Entry (time + runLat) processEvent
>     let newEvents = union (H.fromList [newProcessEvent]) stateEvents
>     let newRunLatency = runLatency + runLat
>     let updatedCpus = S.adjust' (\c -> c - 1) cpu stateCpuQueues
>
>     liftIO $ traceIO ("New events " ++ show [newProcessEvent])
>     return $ state { stateEvents = newEvents
>                    , stateCpuQueues = updatedCpus
>                    , runLatency = newRunLatency }

Now as we finished with the state machine, it's time to describe how to
calculate latencies for every event we use. The following functions specify
various details of the model in this regard.

How long the task spend in the queue. Takes into account CPU contention.
Exponential version would look like this:

     let meanValue = log $ latencyMean (ProcessQueue clock cpu) :: Double
     let stdDev = log $ latencyStdDev (ProcessQueue clock cpu) :: Double
     processing <- liftIO $ (Fu.runRVar (exp <$> normal meanValue stdDev) gen)

> latency (ProcessQueue runLat clock cpu) SimulationState{stateCpuQueues, stateGen} = do
>     -- Normal distribution instead of log normal would look like this
>     let meanValue = latencyMean (ProcessQueue runLat clock cpu) :: Double
>     let stdDev = latencyStdDev (ProcessQueue runLat clock cpu) :: Double
>     processing <- liftIO $ (Fu.runRVar (normal meanValue stdDev) stateGen)
>
>     -- it takes longer with contention on the cpu
>     let contention = case (S.lookup cpu stateCpuQueues) of
>             Just cpuLoad -> if cpuLoad > 4
>                             then fromIntegral (cpuLoad - 4)
>                             else 1
>             Nothing      -> 1
>     return $ round (processing * contention)

How much time the task spends doing its job

> latency (ProcessRun clock cpu) SimulationState{stateCpuQueues, stateGen} = do
>     -- Normal distribution instead of log normal would look like this:
>     let meanValue = latencyMean (ProcessRun clock cpu) :: Double
>     let stdDev = latencyStdDev (ProcessRun clock cpu) :: Double
>     processing <- liftIO $ (Fu.runRVar (normal meanValue stdDev) stateGen)
>
>     -- slice of time is getting smaller with contention
>     let contention = case (S.lookup cpu stateCpuQueues) of
>             Just cpuLoad -> if cpuLoad > 4
>                             then fromIntegral (cpuLoad - 4)
>                             else 1
>             Nothing      -> 1
>
>     return $ round (processing / contention)

Latency of entry BPF processing. Incluses BPF map contention.

> latency (BpfQueue cpu) SimulationState{mapSize, stateGen} = do
>     let meanValue = latencyMean (BpfQueue cpu) :: Double
>     let stdDev = latencyStdDev (BpfQueue cpu) :: Double
>     processing <- liftIO $ (Fu.runRVar (normal meanValue stdDev) stateGen)

In case of bpf hash map it's getting slower to work when number of record is
growing. Removing the contention multiplier gives us task local storage case.

>     let bpfMapContention = 1 + (fromIntegral mapSize / 1024) ^ 2
>
>     return $ round (processing * bpfMapContention)

Latency of exit BPF processing. Includes BPF map contention as
well.

> latency (BpfReport cpu) SimulationState{mapSize, stateGen} = do
>     let meanValue = latencyMean (BpfReport cpu) :: Double
>     let stdDev = latencyStdDev (BpfReport cpu) :: Double
>     processing <- liftIO $ (Fu.runRVar (normal meanValue stdDev) stateGen)

In case of bpf hash map it's getting slower to work when number of record is
growing. Removing the contention multiplier gives us task local storage case.

>     let bpfMapContention = 1 + (fromIntegral mapSize / 1024) ^ 2
>
>     return $ round (processing * bpfMapContention)

Latency of BPF reporting, to be more precise bpf_ringbuf_submit. If over the
threshold, processing is increased by perf output

> latency (BpfPerfOutput queueLat cpu) SimulationState{mapSize, stateGen} = do
>     let perfMeanValue = latencyMean (BpfPerfOutput queueLat cpu) :: Double
>     let perfStdDev = latencyStdDev (BpfPerfOutput queueLat cpu) :: Double
>     bpfPerfOutput <- liftIO $ (Fu.runRVar (normal perfMeanValue perfStdDev) stateGen)
>
>     return $ round bpfPerfOutput

Latency of spending waiting to be enqueued.

> latency (ProcessDequeue clock cpu) SimulationState{mapSize, stateGen} = do
>     let meanValue = latencyMean (ProcessDequeue clock cpu) :: Double
>     let stdDev = latencyStdDev (ProcessDequeue clock cpu) :: Double
>     processing <- liftIO $ (Fu.runRVar (normal meanValue stdDev) stateGen)
>     return $ round processing

Constant distribution parameters for various latencies.

> latencyMean (ProcessQueue _ _ _) = 4
> latencyMean (BpfQueue _) = 400
> latencyMean (BpfReport _) = 200

Taken from sched_min_granularity_ns

> latencyMean (ProcessRun _ _) = 3000000
> latencyMean (ProcessDequeue _ _) = 4
> latencyMean (BpfPerfOutput _ _) = 24
>
> latencyStdDev (ProcessQueue _ _ _) = 1
> latencyStdDev (BpfQueue _) = 10
> latencyStdDev (BpfReport _) = 10
> latencyStdDev (ProcessRun _ _) = 1000
> latencyStdDev (ProcessDequeue _ _) = 1
> latencyStdDev (BpfPerfOutput _ _) = 8

Utility functions.

> decrementClock (ProcessRun clock assignedCpu) delta =
>     ProcessRun (max 0 (clock - delta)) assignedCpu

> showOverhead state = putStrLn $ show (bpfRuntime ./. execRuntime)
>   where
>     bpfRuntime = bpfLatency state
>     execRuntime = runLatency state
>
> getDoubleArg :: IO Double
> getDoubleArg = fmap (read . head) getArgs
>
> (./.) x y = (fromIntegral x) / (fromIntegral y)
