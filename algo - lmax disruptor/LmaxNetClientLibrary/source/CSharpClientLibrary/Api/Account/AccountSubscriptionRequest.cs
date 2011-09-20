using Com.Lmax.Api.Internal.Xml;

namespace Com.Lmax.Api.Account
{
    /// <summary>
    /// Subscribe to all requests for account information.
    /// </summary>
    public class AccountSubscriptionRequest : SubscriptionRequest
    {
        protected override void WriteSubscriptionBodyTo(IStructuredWriter writer)
        {
            writer.ValueOrEmpty("type", "account");
        }
    }
}
