using System.IO;
using System.Xml;

namespace Com.Lmax.Api.Internal
{
    public class SaxParser : IXmlParser
    {
        public void Parse(TextReader reader, ISaxContentHandler saxContentHandler)
        {
            XmlReaderSettings settings = new XmlReaderSettings();
            settings.ConformanceLevel = ConformanceLevel.Fragment;

            XmlReader xmlReader = XmlReader.Create(reader, settings);
            while(xmlReader.Read())
            {
                if (xmlReader.HasValue)
                {
                    saxContentHandler.Content(xmlReader.Value);
                }
                else 
                {
                    if (xmlReader.IsEmptyElement)
                    {
                        saxContentHandler.StartElement(xmlReader.Name);
                        saxContentHandler.EndElement(xmlReader.Name);
                    }
                    else if (xmlReader.IsStartElement())
                    {
                        saxContentHandler.StartElement(xmlReader.Name);
                    }
                    else
                    {
                        saxContentHandler.EndElement(xmlReader.Name);
                    }
                }
            }
        }
    }
}
