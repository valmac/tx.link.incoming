using System;
using System.IO;
using System.Net;
using System.Reflection;
using System.Text;
using System.Threading;
using Com.Lmax.Api.Internal.Xml;

namespace Com.Lmax.Api.Internal
{
    public class HttpInvoker : IHttpInvoker
    {
        [ThreadStatic] private static XmlStructuredWriter _writer;
        private readonly string _userAgent;
        private const string SessionCookieName = "JSESSIONID";

        static HttpInvoker()
        {
            ServicePointManager.Expect100Continue = false;
            ServicePointManager.UseNagleAlgorithm = false;
            ServicePointManager.DefaultConnectionLimit = 5;
        }

        public HttpInvoker() : 
            this("")
        {
        }

        public HttpInvoker(string clientIdentifier)
        {
            _userAgent = "LMAX .Net API, version: " + Assembly.GetExecutingAssembly().GetName().Version + ", id: " +
                         clientIdentifier;
        }

        public virtual Response Invoke(string baseUri, IRequest request, IXmlParser xmlParser, Handler handler, out string sessionId)
        {
            HttpWebRequest webRequest = CreateWebRequest(baseUri, request);
            webRequest.CookieContainer = new CookieContainer();
            setUserAgent(webRequest);
            WriteRequest(webRequest, request);

            HttpWebResponse webResponse = ReadResponse(webRequest, xmlParser, handler);
            Cookie jsessionId = webResponse.Cookies[SessionCookieName];
            if (null == jsessionId)
            {
                string cookieToSet = webResponse.Headers["Set-Cookie"];
                if (cookieToSet != null && cookieToSet.StartsWith(SessionCookieName))
                {
                    sessionId = cookieToSet.Substring(SessionCookieName.Length + 1);
                    sessionId = sessionId.Substring(0, sessionId.IndexOf(';'));

                }
                else
                {
                    // Can't throw an exception as the login response might contain a warning with no JSESSIONID.
                    // throw new Exception("Unable to locate JSESSIONID Cookie");
                    sessionId = null;                    
                }
            }
            else
            {
                sessionId = jsessionId.Value;
            }

            return new Response(webResponse.StatusCode);
        }

        public virtual Response PostInSession(string baseUri, IRequest request, IXmlParser xmlParser, Handler handler, string sessionId)
        {
            HttpWebRequest webRequest = SendRequest(baseUri, request, sessionId);

            HttpWebResponse webResponse = ReadResponse(webRequest, xmlParser, handler);

            return new Response(webResponse.StatusCode);
        }

        public Response GetInSession(string baseUri, IRequest request, IXmlParser xmlParser, Handler handler, string sessionId)
        {
            if (null == sessionId)
            {
                throw new ArgumentException("'sessionId' must not be null");
            }

            HttpWebRequest webRequest = (HttpWebRequest)WebRequest.Create(baseUri + request.Uri);
            setUserAgent(webRequest);
            webRequest.Method = "GET";
            webRequest.Accept = "text/xml";
            webRequest.Headers.Add("Cookie", SessionCookieName + "=" + sessionId);

            HttpWebResponse webResponse = ReadResponse(webRequest, xmlParser, handler);

            return new Response(webResponse.StatusCode);
        }


        public IConnection Connect(Uri uri, string sessionId)
        {
            HttpWebRequest webRequest = CreateBinaryGetRequest(uri);
            webRequest.Headers.Add("Cookie", SessionCookieName + "=" + sessionId);
            try
            {
                HttpWebResponse webResponse = (HttpWebResponse)webRequest.GetResponse();
                return new Connection(webRequest, webResponse);

            }
            catch (WebException e)
            {
                throw new UnexpectedHttpStatusCodeException(((HttpWebResponse)e.Response).StatusCode);
            }

        }

        public IConnection Connect(string baseUri, IRequest request, string sessionId)
        {
            HttpWebRequest webRequest = SendRequest(baseUri, request, sessionId);
            try
            {
                HttpWebResponse webResponse = (HttpWebResponse)webRequest.GetResponse();
                return new Connection(webRequest, webResponse);
                
            } 
            catch (WebException e)
            {
                throw new UnexpectedHttpStatusCodeException(((HttpWebResponse)e.Response).StatusCode);                
            }

        }

        private static XmlStructuredWriter Writer
        {
            get { return _writer ?? (_writer = new XmlStructuredWriter()); }
        }

        private HttpWebRequest SendRequest(string baseUri, IRequest request, string sessionId)
        {
            if (null == sessionId)
            {
                throw new ArgumentException("'sessionId' must not be null");
            }

            HttpWebRequest webRequest = CreateWebRequest(baseUri, request);
            webRequest.Headers.Add("Cookie", SessionCookieName + "=" + sessionId);

            setUserAgent(webRequest);

            WriteRequest(webRequest, request);
            return webRequest;
        }

        private HttpWebResponse ReadResponse(HttpWebRequest webRequest, IXmlParser xmlParser, Handler handler)
        {
            HttpWebResponse webResponse = (HttpWebResponse)webRequest.GetResponse();

            using (StreamReader reader = new StreamReader(webResponse.GetResponseStream(), new UTF8Encoding()))
            {
                xmlParser.Parse(reader, new SaxContentHandler(handler));
            }

            return webResponse;
        }

        private static void WriteRequest(HttpWebRequest webRequest, IRequest request)
        {
            request.WriteTo(Writer);            

            using (Stream oStreamOut = webRequest.GetRequestStream())
            {
                Writer.WriteTo(oStreamOut);
            }

            Writer.Reset();
        }

        private HttpWebRequest CreateBinaryGetRequest(Uri uri)
        {
            HttpWebRequest webRequest = (HttpWebRequest)WebRequest.Create(uri);
            webRequest.Method = "GET";
            webRequest.Accept = "*/*";
            setUserAgent(webRequest);
            return webRequest;
        }

        private HttpWebRequest CreateWebRequest(string baseUri, IRequest request)
        {
            HttpWebRequest webRequest = (HttpWebRequest)WebRequest.Create(baseUri + request.Uri);
            webRequest.Method = "POST";
            webRequest.ContentType = "text/xml";
            webRequest.Accept = "text/xml";
            return webRequest;
        }

        private void setUserAgent(HttpWebRequest webRequest)
        {
            webRequest.UserAgent = _userAgent;
        }

    }
}
