import java.util.concurrent.*;

public class Simulator{
    
    private String simulationData;
    
    /*
     * param1 to param3 represent parameters for simulation, these will be used by SimulateWorker for the simulation
     * 
     */
    public String simulate(double param1, double param2, double param3)
    {
       ExecutorService executor = Executors.newSingleThreadExecutor();
       Future<String> future = executor.submit(new SimulatorTask(param1, param2, param3));
       
       try
       {
           simulationData = future.get();
       }
       catch(InterruptedException | ExecutionException e)
       {
           System.out.println(e);
       }
       executor.shutdownNow();
       return simulationData;
    }
    
    private class SimulatorTask implements Callable<String>
    {
        private String data;
        private StringBuilder sb;
        private double param1;
        private double param2;
        private double param3;
        
        public SimulatorTask(double param1, double param2, double param3)
        {
            this.param1 = param1;
            this.param2 = param2;
            this.param3 = param3;
            
            sb = new StringBuilder();
        }
        @Override
        public String call() throws Exception
        {
            return simulation();
        }
        
        // Simulation logic goes here...
        public String simulation()
        {   
            // Use time as test data
            long startTime = System.currentTimeMillis();
            
            String names = "StartTime CurrentTime Elapsed Params";
            String params = param1 +"-"+param2+"-"+param3; // to illustrate parameters are used
            
            sb.append(names);
            
            // simulate 10000 records
            for (int i = 0; i < 10000; i++) {
                sb.append("\n");
                sb.append(startTime).append(" ").append(System.currentTimeMillis()).append(" ").append(System.currentTimeMillis()-startTime).append(" ").append(params);
            }
            data = sb.toString();
            return data;
        }
    }
}
