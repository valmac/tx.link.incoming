using System;
using Com.Lmax.Api.Internal.Xml;

namespace Com.Lmax.Api
{
    /// <summary>
    /// Contains a of the necessary credential information and product type required
    /// connect to the LMAX Trader platform.
    /// </summary>
    public class LogoutRequest : IRequest
    {
        private const string LogoutUri = "/public/security/logout";

        /// <summary>
        /// The URI for the login request. 
        /// </summary>
        public string Uri { get { return LogoutUri; } }

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
