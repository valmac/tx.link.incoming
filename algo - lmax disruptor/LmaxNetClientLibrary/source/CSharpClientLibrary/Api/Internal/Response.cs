using System.Net;

namespace Com.Lmax.Api.Internal
{
    public class Response
    {
        private readonly HttpStatusCode _statusCode;

        public Response(HttpStatusCode statusCode)
        {
            _statusCode = statusCode;
        }

        public bool IsOk
        {
            get { return _statusCode == HttpStatusCode.OK; }
        }

        public int Status
        {
            get { return (int) _statusCode; }
        }
    }
}
