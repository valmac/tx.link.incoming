using System.Text;

namespace Com.Lmax.Api.Internal
{
    public class Handler
    {
        protected const string OK = "OK";
        protected const string BODY = "body";
        protected const string MESSAGE = "message";
        protected const string STATUS = "status";
        
        private readonly string _elementName;
        private StringBuilder _contentBuilder = new StringBuilder();
        
        public Handler() : this(null)
        {
        }

        public Handler(string elementName)
        {
            _elementName = elementName;
        }

        public virtual bool IsOk
        {
            get { return false; }
        }

        public virtual string Message
        {
            get { return ""; }
        }

        public virtual string ElementName
        {
            get { return _elementName; }
        }

        public virtual string Content
        {
            get { return _contentBuilder.ToString(); }
        }

        public virtual Handler GetHandler(string qName)
        {
            return this;
        }

        public virtual void Characters(string characterData, int start, int length)
        {
            _contentBuilder.Append(characterData, start, length);
        }

        public virtual void Reset(string element)
        {
            _contentBuilder = new StringBuilder();
        }

        public virtual void EndElement(string endElement)
        {
        }
    }
}
