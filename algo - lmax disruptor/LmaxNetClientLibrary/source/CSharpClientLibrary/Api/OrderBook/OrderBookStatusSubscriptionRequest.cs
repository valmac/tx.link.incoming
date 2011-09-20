using Com.Lmax.Api.Internal.Xml;

namespace Com.Lmax.Api.OrderBook
{
    /// <summary>
    /// Used when subscribing to the order book status events.
    /// </summary>
    public class OrderBookStatusSubscriptionRequest : SubscriptionRequest
    {
        private readonly long _instrumentId;
  
        /// <summary>
        /// Construct the OrderBookStatusSubscriptionRequest. 
        /// </summary>
        /// <param name="instrumentId">
        /// A <see cref="System.Int64"/> that is the instrument id of the 
        /// order book that you are intrested in seeing status events for.
        /// </param>
        public OrderBookStatusSubscriptionRequest(long instrumentId)
        {
            _instrumentId = instrumentId;
        }

        protected override void WriteSubscriptionBodyTo(IStructuredWriter writer)
        {
            writer.ValueOrEmpty("orderBookStatus", _instrumentId);
        }
    }
}
