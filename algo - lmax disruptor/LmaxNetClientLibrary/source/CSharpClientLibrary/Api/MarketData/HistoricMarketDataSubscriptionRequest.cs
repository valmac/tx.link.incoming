using Com.Lmax.Api.Internal.Xml;

namespace Com.Lmax.Api.MarketData
{
    ///<summary>
    /// Used when subscribing to the events containing historic market data responses.
    ///</summary>
    public class HistoricMarketDataSubscriptionRequest : ISubscriptionRequest
    {
            /// <summary>
            /// Readonly property containing the URI for the request. 
            /// </summary>
            public string Uri
            {
                get { return "/secure/subscribe"; }
            }

            /// <summary>
            /// Internal: Output this request.  Can't use the Abstract Subscription Request because it needs to append two separate subscriptions.
            /// </summary>
            /// <param name="writer">The destination for the request content</param>
            public void WriteTo(IStructuredWriter writer)
            {
                writer.StartElement("req").StartElement("body");
                writer.StartElement("subscription");
                writer.ValueOrEmpty("type", "historicMarketData");
                writer.EndElement("subscription").EndElement("body").EndElement("req");
            }

    }
}