namespace Com.Lmax.Api.Internal.Protocol
{
    public class EventHandler : Handler
    {
        private readonly OrderBookEventHandler _orderBookEventHandler;

        // exposed for testing
        public EventHandler(OrderBookEventHandler orderBookEventHandler)
        {
            _orderBookEventHandler = orderBookEventHandler;
        }

        public override Handler GetHandler(string qName)
        {
            if (qName == _orderBookEventHandler.ElementName)
            {
                return _orderBookEventHandler;
            }
            return this;
        }
    }
}

