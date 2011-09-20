using System;
using System.IO;
using System.Net;
using System.Text;

namespace Com.Lmax.Api.Internal
{
    public class Connection : IConnection
    {
        private readonly WebRequest _webRequest;
        private readonly WebResponse _webResponse;

        public Connection(WebRequest webRequest, WebResponse webResponse)
        {
            _webRequest = webRequest;
            _webResponse = webResponse;
        }

        public TextReader GetTextReader()
        {
            return new StreamReader(_webResponse.GetResponseStream(), new UTF8Encoding());
        }

        public BinaryReader GetBinaryReader()
        {
            return new BinaryReader(_webResponse.GetResponseStream());
        }

        public void Abort()
        {
            _webRequest.Abort();
        }
    }
}
