using Com.Lmax.Api.Internal.Xml;

namespace Com.Lmax.Api
{
    /// <summary>
    /// Base subscription request.
    /// </summary>
    public abstract class SubscriptionRequest : ISubscriptionRequest
    {
        /// <summary>
        /// Readonly property containing the URI for the request. 
        /// </summary>
        public string Uri
        {
            get { return "/secure/subscribe"; }
        }

        /// <summary>
        /// Internal: Output this request.
        /// </summary>
        /// <param name="writer">The destination for the request content</param>
        public void WriteTo(IStructuredWriter writer)
        {
            writer.StartElement("req").StartElement("body").StartElement("subscription");
            WriteSubscriptionBodyTo(writer);
            writer.EndElement("subscription").EndElement("body").EndElement("req");
        }

        protected abstract void WriteSubscriptionBodyTo(IStructuredWriter writer);
    }
}