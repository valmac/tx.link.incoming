using System.IO;

namespace Com.Lmax.Api.Internal
{
    public class EventStreamHandler
    {
        private readonly ISaxContentHandler _saxContentHandler;

        // for testing only
        public EventStreamHandler() : this(null)
        {
        }

        public EventStreamHandler(ISaxContentHandler saxContentHandler)
        {
            _saxContentHandler = saxContentHandler;
        }

        public virtual void ParseEventStream(TextReader reader)
        {
            new SaxParser().Parse(reader, _saxContentHandler);
        }
    }
}
