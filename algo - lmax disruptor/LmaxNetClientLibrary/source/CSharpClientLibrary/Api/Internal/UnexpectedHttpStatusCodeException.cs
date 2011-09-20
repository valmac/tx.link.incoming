using System;
using System.Net;

namespace Com.Lmax.Api.Internal
{
    public class UnexpectedHttpStatusCodeException : Exception
    {
        private readonly HttpStatusCode _statusCode;

        public UnexpectedHttpStatusCodeException(HttpStatusCode statusCode)
        {
            _statusCode = statusCode;
        }

        public HttpStatusCode StatusCode
        {
            get
            {
                return _statusCode;
            }
        }
    }
}
