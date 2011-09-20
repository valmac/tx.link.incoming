using Com.Lmax.Api.Internal.Xml;

namespace Com.Lmax.Api.Account
{
    /// <summary>
    /// A request for the current account state
    /// </summary>
    public class AccountStateRequest : IRequest
    {
        public string Uri
        {
            get { return "/secure/account/requestAccountState"; }
        }

        /// <summary>
        /// Internal: Output this request.
        /// </summary>
        /// <param name="writer">The destination for the content of this request</param>
        public void WriteTo(IStructuredWriter writer)
        {
            writer.
                StartElement("req").
                    WriteEmptyTag("body").
                EndElement("req");
        }
    }
}
