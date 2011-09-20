using System;
using System.Threading;
using Com.Lmax.Api.Account;

namespace Com.Lmax.Api
{
    class HeartbeatClient
    {
        private ISession _session;
        private long _nextHeartbeatId;

        private void HeartbeatEvent(String token)
        {
            Console.WriteLine("Received heartbeat: {0}", token);
        }

        private void RunHeartbeatClient()
        {
            try
            {
                while (Thread.CurrentThread.IsAlive)
                {
                    Thread.Sleep(2000);

                    RequestHeartbeat();
                }
            }
            catch (ThreadInterruptedException e)
            {
                Console.WriteLine(e.StackTrace);
            }
        }

        private void RequestHeartbeat()
        {
            string token = "token-" + _nextHeartbeatId++;
            Console.WriteLine("Requesting heartbeat: " + token);

            _session.RequestHeartbeat(new HeartbeatRequest(token),
                () => Console.WriteLine("Successfully requested heartbeat: " + token),
                failureResponse => { throw new Exception("Failed"); });
        }

        private void OnLoginSuccess(ISession session)
        {
            Console.WriteLine("My accountId is: " + session.AccountDetails.AccountId);

            _session = session;
            _session.HeartbeatReceived += HeartbeatEvent;

            session.Subscribe(new HeartbeatSubscriptionRequest(), () => Console.WriteLine("Subscribed to heartbeat"), FailureCallback("subscribe to heartbeats"));

            Thread thread = new Thread(RunHeartbeatClient);
            thread.IsBackground = true;

            thread.Start();

            session.Start();
        }

        private static OnFailure FailureCallback(string failedFunction)
        {
            return
                failureResponse =>
                Console.Error.WriteLine("Failed to " + failedFunction + " due to: " + failureResponse.Message);
        }

        public static void Main(string[] args)
        {
            HeartbeatClient loginClient = new HeartbeatClient();
            if (args.Length != 4)
            {
                Console.WriteLine("Usage " + loginClient.GetType().Name + " <url> <username> <password> [CFD_DEMO|CFD_LIVE]");
                Environment.Exit(-1);
            }

            String url = args[0];
            String username = args[1];
            String password = args[2];
            ProductType productType = (ProductType)Enum.Parse(typeof(ProductType), args[3].ToUpper());

            LmaxApi lmaxApi = new LmaxApi(url);

            lmaxApi.Login(new LoginRequest(username, password, productType), loginClient.OnLoginSuccess, FailureCallback("log in"));
        }
    }

}
