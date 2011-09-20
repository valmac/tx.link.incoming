using System.IO;

namespace Com.Lmax.Api.Internal
{
    public interface IConnection
    {
        TextReader GetTextReader();

        BinaryReader GetBinaryReader();

        void Abort();
    }
}