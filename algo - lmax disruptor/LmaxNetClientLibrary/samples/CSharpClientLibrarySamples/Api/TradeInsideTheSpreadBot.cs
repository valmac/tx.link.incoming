using System;
using System.Collections.Generic;
using System.Threading;
using Com.Lmax.Api.Order;
using Com.Lmax.Api.OrderBook;
using Com.Lmax.Api.Reject;

namespace Com.Lmax.Api
{
    public class TradeInsideTheSpreadBot
    {
        private ISession _session;
        private readonly long _instrumentId;
        private readonly decimal _tickSize;
        private int _nextInstructionId = new Random().Next();

        private readonly OrderTracker _buyOrderTracker = new OrderTracker();
        private readonly OrderTracker _sellOrderTracker = new OrderTracker();

        private TradeInsideTheSpreadBot(long instrumentId, decimal tickSize)
        {
            _instrumentId = instrumentId;
            _tickSize = tickSize;
        }

        private void OrderEventListener(Order.Order order)
        {
            OrderState stateForOrder = GetStateForOrder(order);
            Console.WriteLine("State for order: {0:d}, state: {1}", order.InstructionId, stateForOrder);

            if (order.InstructionId == _buyOrderTracker.InstructionId)
            {
                _buyOrderTracker.OrderState = stateForOrder;
            }
            else if (order.InstructionId == _sellOrderTracker.InstructionId)
            {
                _sellOrderTracker.OrderState = stateForOrder;
            }
        }

        private static OrderState GetStateForOrder(Order.Order order)
        {
            if (order.CancelledQuantity == 0 && order.FilledQuantity == 0 && order.Quantity != 0)
            {
                return OrderState.Working;
            }
            if (order.FilledQuantity != 0)
            {
                return OrderState.Filled;
            }

            return OrderState.Unknown;
        }

        private void FailOnInstructionRejected(InstructionRejectedEvent instructionRejected)
        {
            long instructionId = instructionRejected.InstructionId;
            Console.Error.WriteLine("Instruction " + instructionId + " rejected: " + instructionRejected.Reason);

            if (_buyOrderTracker.CancelInstructionId == instructionId)
            {
                _buyOrderTracker.Reset();
            }
            else if (_sellOrderTracker.CancelInstructionId == instructionId)
            {
                _sellOrderTracker.Reset();
            }
        }

        private void MarketDataUpdate(OrderBookEvent onOrderBookEvent)
        {
            Console.WriteLine(onOrderBookEvent);

            // React to price updates from the exchange.
            HandleBidPrice(onOrderBookEvent.BidPrices);
            HandleAskPrice(onOrderBookEvent.AskPrices);
        }

        private void HandleAskPrice(List<PricePoint> askPrices)
        {
            HandlePriceChange(askPrices, _sellOrderTracker, -1m, -_tickSize);
        }

        private void HandleBidPrice(List<PricePoint> bidPrices)
        {
            HandlePriceChange(bidPrices, _buyOrderTracker, 1m, _tickSize);
        }

        private void HandlePriceChange(List<PricePoint> prices, OrderTracker orderTracker, decimal quantity,
                                       decimal priceDelta)
        {
            decimal mostRecentOrderPrice = orderTracker.Price;
            if (prices.Count == 0)
            {
                //if there's no best price, we can't decide what price to place, so exit
                return;
            }
            PricePoint bestPriceInTheMarket = prices[0];

            // Make sure we have a best bid price, and it's not the same as the order we just placed
            // and place similar to the ask price change.
            if (mostRecentOrderPrice == 0 || mostRecentOrderPrice != bestPriceInTheMarket.Price)
            {
                switch (orderTracker.OrderState)
                {
                    // Place an order inside the spread if there isn't one currently in the market
                    case OrderState.None:
                        orderTracker.Price = bestPriceInTheMarket.Price + priceDelta;

                        orderTracker.InstructionId = NextInstructionId();
                        LimitOrderSpecification order =
                            new LimitOrderSpecification(orderTracker.InstructionId, _instrumentId, orderTracker.Price,
                                                        quantity, TimeInForce.GoodForDay);

                        _session.PlaceLimitOrder(order,
                                                 OrderSuccessCallback(orderTracker),
                                                 FailureCallback("Place order failed for instruction ID " +
                                                                 orderTracker.InstructionId));
                        break;

                    // Cancel a working order on a price change.
                    case OrderState.Working:
                        CancelOrder(orderTracker);
                        break;
                }
            }
        }

        private static OnInstructionResponse OrderSuccessCallback(OrderTracker orderTracker)
        {
            return instructionId =>
                       {
                           Console.WriteLine("order placed with instruction id " + instructionId);
                           orderTracker.OrderState = OrderState.Working;
                       };
        }

        private void CancelOrder(OrderTracker orderTracker)
        {
            long originalInstructionId = orderTracker.InstructionId;

            if (originalInstructionId != -1)
            {
                orderTracker.CancelInstructionId = NextInstructionId();
                CancelOrderRequest cancelOrderRequest = new CancelOrderRequest(orderTracker.CancelInstructionId,
                                                                               _instrumentId,
                                                                               originalInstructionId);
                _session.CancelOrder(cancelOrderRequest,
                                     instructionId =>
                                     Console.WriteLine("instruction id " + orderTracker.CancelInstructionId +
                                                       " sent to cancel order " + originalInstructionId),
                                     FailureCallback("Cancel order with cancelOrderInstructionId: " +
                                                     orderTracker.CancelInstructionId));
            }
        }

        private static OnFailure FailureCallback(string failedFunction)
        {
            return
                failureResponse =>
                Console.Error.WriteLine("Failed to " + failedFunction + " due to: " + failureResponse.Message);
        }

        private static OnSuccess SubscribeCallback(string message)
        {
            return () => Console.WriteLine("Subscribed to " + message);
        }

        private long NextInstructionId()
        {
            // Needs to be prefixed with the 'ref' keyword.
            return Interlocked.Increment(ref _nextInstructionId);

            // All calls to _nextInstructionId should use methods
            // from the Interlocked class (Get, Set, etc.), otherwise it's
            // not thread safe.
        }

        private void LoginCallback(ISession session)
        {
            Console.WriteLine("My accountId is: " + session.AccountDetails.AccountId);

            // Hold onto the session for later use.
            _session = session;

            // Add listeners for events.
            _session.OrderChanged += OrderEventListener;
            _session.InstructionRejected += FailOnInstructionRejected;
            _session.MarketDataChanged += MarketDataUpdate;

            // Subscribe to my order events.
            _session.Subscribe(new OrderSubscriptionRequest(), SubscribeCallback("orders"),
                               FailureCallback("subscribe to orders"));

            // Subscribe to the order book that I'm interested in.
            _session.Subscribe(new OrderBookSubscriptionRequest(_instrumentId),
                               SubscribeCallback("instrument: " + _instrumentId),
                               FailureCallback("subscribe to order book " + _instrumentId));

            // Start the event processing loop, this method will block until the session is stopped.
            session.Start();
        }

        public static void Main(string[] args)
        {
            TradeInsideTheSpreadBot loginClient = new TradeInsideTheSpreadBot(4012, 0.00001m);
            if (args.Length != 4)
            {
                Console.WriteLine("Usage: " + loginClient.GetType() + " <url> <username> <password> [CFD_DEMO|CFD_LIVE]");
                Environment.Exit(-1);
            }

            string url = args[0];
            string username = args[1];
            string password = args[2];
            ProductType productType = (ProductType)Enum.Parse(typeof(ProductType), args[3].ToUpper());

            LmaxApi lmaxApi = new LmaxApi(url);

            lmaxApi.Login(new LoginRequest(username, password, productType), loginClient.LoginCallback,
                          FailureCallback("log in"));
        }
    }

    internal enum OrderState
    {
        None,
        Working,
        Filled,
        Unknown
    }

    internal class OrderTracker
    {
        private long _instructionId = -1;
        private long _cancelInstructionId = -1;
        private OrderState _orderState = OrderState.None;

        public long InstructionId
        {
            get { return _instructionId; }
            set { _instructionId = value; }
        }

        public long CancelInstructionId
        {
            get { return _cancelInstructionId; }
            set { _cancelInstructionId = value; }
        }

        public OrderState OrderState
        {
            get { return _orderState; }
            set { _orderState = value; }
        }

        public decimal Price { get; set; }

        public void Reset()
        {
            _instructionId = -1;
            _cancelInstructionId = -1;
            _orderState = OrderState.None;
        }
    }
}