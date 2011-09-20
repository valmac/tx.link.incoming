namespace Com.Lmax.Api.Internal
{
    public interface ISaxContentHandler
    {
        void StartElement(string tagName);
        void Content(string value);
        void EndElement(string tagName);
    }
}

