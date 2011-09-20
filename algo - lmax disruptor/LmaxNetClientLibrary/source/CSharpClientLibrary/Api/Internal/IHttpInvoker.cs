using System;

namespace Com.Lmax.Api.Internal
{
    public interface IHttpInvoker
    {
        Response Invoke(string baseUri, IRequest request, IXmlParser xmlParser, Handler handler, out string sessionId);

        Response PostInSession(string baseUri, IRequest request, IXmlParser xmlParser, Handler handler, string sessionId);

        Response GetInSession(string baseUri, IRequest request, IXmlParser xmlParser, Handler handler, string sessionId);
        
        IConnection Connect(string baseUri, IRequest request, string sessionId);

        IConnection Connect(Uri uri, string sessionId);
    }
}