using Com.Lmax.Api.Internal.Xml;

namespace Com.Lmax.Api.OrderBook
{
    /// <summary>
    /// Used when subscribing to the events for an OrderBook 
    /// </summary>
    public class OrderBookSubscriptionRequest : SubscriptionRequest
    {
        private readonly long _instrumentId;
  
        /// <summary>
        /// Construct the OrderBookSubscriptionRequest. 
        /// </summary>
        /// <param name="instrumentId">
        /// A <see cref="System.Int64"/> that is the instrumentId of the 
        /// OrderBook that you are intrested in seeing events for.
        /// </param>
        public OrderBookSubscriptionRequest(long instrumentId)
        {
            _instrumentId = instrumentId;
        }

        protected override void WriteSubscriptionBodyTo(IStructuredWriter writer)
        {
            writer.ValueOrEmpty("ob2", _instrumentId);
        }
    }
}
