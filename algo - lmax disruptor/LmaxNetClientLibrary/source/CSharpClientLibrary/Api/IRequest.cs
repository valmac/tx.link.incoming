using Com.Lmax.Api.Internal.Xml;

namespace Com.Lmax.Api
{
    /// <summary>
    /// Common base type for all requests. 
    /// </summary>
    public interface IRequest
    {
        /// <summary>
        /// Readonly property containing the URI for the request. 
        /// </summary>
        string Uri { get; }

        /// <summary>
        /// Internal: Output this request.
        /// </summary>
        /// <param name="writer">The destination for the request content</param>
        void WriteTo(IStructuredWriter writer);
    }
}

