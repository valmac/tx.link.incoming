using Com.Lmax.Api.Internal.Xml;

namespace Com.Lmax.Api.Order
{
    /// <summary>
    ///   Request to subscribe to order events.
    /// </summary>
    public class OrderSubscriptionRequest : SubscriptionRequest
    {
        protected override void WriteSubscriptionBodyTo(IStructuredWriter writer)
        {
            writer.ValueOrEmpty("type", "order");
        }
    }
}