using System;
using System.Collections.Generic;
using System.Threading;
using Com.Lmax.Api.Order;
using Com.Lmax.Api.OrderBook;

namespace Com.Lmax.Api
{
    class LoginClient
    {
        private ISession _session;
        private readonly List<long> _orderInstructions = new List<long>();
        private readonly long _instrumentId;
        private int _nextInstructionId = new Random().Next();

        private LoginClient(long instrumentId)
        {
            _instrumentId = instrumentId;
        }

        private void MarketDataUpdate(OrderBookEvent orderBookEvent)
        {
            long placeOrderInstructionId = NextInstructionId();
            _orderInstructions.Add(placeOrderInstructionId);
            _session.PlaceLimitOrder(new LimitOrderSpecification(placeOrderInstructionId, _instrumentId, 10m, 10m, TimeInForce.GoodForDay),
                    instructionId => Console.WriteLine("limit order placed with instruction id " + instructionId),
                                                     FailureCallback("Place order failed for instruction ID " + placeOrderInstructionId));
        }

        private void LoginCallback(ISession session)
        {
            Console.WriteLine("My accountId is: " + session.AccountDetails.AccountId);

            _session = session;
            _session.MarketDataChanged += MarketDataUpdate;

            _session.Subscribe(new OrderBookSubscriptionRequest(_instrumentId),
                               () => Console.WriteLine("Subscribed to " + ("instrument: " + _instrumentId)),
                               FailureCallback("subscribe to order book " + _instrumentId));

            _session.Start();
        }

        private long NextInstructionId()
        {
            return Interlocked.Increment(ref _nextInstructionId);
        }

        private static OnFailure FailureCallback(string failedFunction)
        {
            return failureResponse =>
                Console.Error.WriteLine("Failed to " + failedFunction + " due to: " + failureResponse.Message);
        }

        public static void Main(string[] args)
        {
            LoginClient loginClient = new LoginClient(4001);
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
            lmaxApi.Login(new LoginRequest(username, password, productType), loginClient.LoginCallback, FailureCallback("log in"));
        }
    }

}
