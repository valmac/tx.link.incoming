using Com.Lmax.Api.Internal.Xml;

namespace Com.Lmax.Api.Position
{
    /// <summary>
    /// Request to subscribe to position events.
    /// </summary>
    public class PositionSubscriptionRequest : SubscriptionRequest
    {
        protected override void WriteSubscriptionBodyTo(IStructuredWriter writer)
        {
            writer.ValueOrEmpty("type", "position");
        }
    }
}
