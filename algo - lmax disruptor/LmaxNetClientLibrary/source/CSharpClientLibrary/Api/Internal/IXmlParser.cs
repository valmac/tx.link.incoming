using System.IO;

namespace Com.Lmax.Api.Internal
{
    public interface IXmlParser
    {
        void Parse(TextReader reader, ISaxContentHandler saxContentHandler);
    }
}
