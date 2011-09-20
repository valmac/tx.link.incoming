using System;

namespace Com.Lmax.Api.Internal.Xml
{
    public interface IStructuredWriter
    {
        IStructuredWriter StartElement(string name);
        IStructuredWriter EndElement(string name);

        IStructuredWriter WriteEmptyTag(string name);
        
        IStructuredWriter ValueUTF8(string name, string value);
        
        IStructuredWriter ValueOrEmpty(string name, string value);
        IStructuredWriter ValueOrNone(string name, string value);        
    
        IStructuredWriter ValueOrEmpty(string name, long? value);
        IStructuredWriter ValueOrNone(string name, long? value);
    
        IStructuredWriter ValueOrEmpty(string name, decimal? value);
        IStructuredWriter ValueOrNone(string name, decimal? value);
    
        IStructuredWriter ValueOrEmpty(string name, bool value);            
    }
}
