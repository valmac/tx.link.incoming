using Com.Lmax.Api.Internal.Xml;

namespace Com.Lmax.Api
{
    /// <summary>
    /// Contains the necessary credential information and product type required
    /// connect to the LMAX Trader platform.
    /// </summary>
    public class LoginRequest : IRequest
    {
        private const string ProtocolVersion = "1.8";
        private const string LoginUri = "/public/security/login";        

        private readonly string _username;
        private readonly string _password;
        private readonly ProductType _productType;
  
        /// <summary>
        /// Construct a login request with the appropriate credential and product type 
        /// </summary>
        /// <param name="username">
        /// A <see cref="System.String"/> contains the username.
        /// </param>
        /// <param name="password">
        /// A <see cref="System.String"/> contains the password.
        /// </param>
        /// <param name="productType">
        /// A <see cref="ProductType"/> either CFD_DEMO for testapi and CFD_LIVE for
        /// production.
        /// </param>
        public LoginRequest(string username, string password, ProductType productType)
        {
            _username = username;
            _password = password;
            _productType = productType;
        }

        /// <summary>
        /// Construct a login request with the appropriate credential.  
        /// Product type will default to CFD_LIVE.
        /// </summary>
        /// <param name="username">
        /// A <see cref="System.String"/> contains the username.
        /// </param>
        /// <param name="password">
        /// A <see cref="System.String"/> contains the password.
        /// </param>
        public LoginRequest(string username, string password)
            : this(username, password, ProductType.CFD_LIVE)
        {
        }
  
        /// <summary>
        /// The URI for the login request. 
        /// </summary>
        public string Uri { get { return LoginUri; } }

        /// <summary>
        /// Internal: Output this request.
        /// </summary>
        /// <param name="writer">The destination for the content of this request</param>
        public void WriteTo(IStructuredWriter writer)
        {
            writer.
                StartElement("req").
                    StartElement("body").
                        ValueUTF8("username", _username).
                        ValueUTF8("password", _password).
                        ValueOrNone("protocolVersion", ProtocolVersion).
                        ValueOrNone("productType", _productType.ToString()).
                    EndElement("body").
                EndElement("req");
        }
    }
 
    /// <summary>
    /// The product type used to connect to the LMAX Trader platform. 
    /// </summary>
    public enum ProductType
    {
        ///<summary>
        /// Selected if connecting to the production environment
        ///</summary>
        CFD_LIVE,
        ///<summary>
        /// Selected if connecting to the test environment
        ///</summary>
        CFD_DEMO
    }
}
