using System;
using System.IO;
using System.Text;

namespace XltDecoder
{
  // ************************************************************************
  // *                            XLTable class                             *
  // ************************************************************************

  // Modified code from Emanuele Ruffaldi
  public class XlTable
  {
    const int codepage = 1251;

    // **********************************************************************

    public enum Type
    {
      Unk = 0xFF,
      EOF = 0xF0,
      Table = 0x10,  // 4
      Float = 0x01,  // 8
      String = 0x02,  // 1+N
      Bool = 0x03,  // 2
      Error = 0x04,  // 2
      Blank = 0x05,  // 2
      Int = 0x06,    // 2
      Skip = 0x7,    // 2
    }

    // **********************************************************************

    int intValue;
    double floatValue;
    string strValue;
    Type currentType;
    int blockstart, blocksize;
    int blockitems;
    int pos;
    byte[] data;
    int cols, rows;

    BinaryReader br;

    // **********************************************************************

    public BinaryReader Stream { get { return br; } }
    public int Columns { get { return cols; } }
    public int Rows { get { return rows; } }
    public int BlockItems { get { return blockitems; } }

    public int IntValue { get { return intValue; } }
    public double FloatValue { get { return floatValue; } }
    public string StringValue { get { return strValue; } }

    // **********************************************************************

    public XlTable(byte[] data)
    {
      this.data = data;
      pos = 0;
      blockstart = 0;
      blocksize = -4;
      br = new BinaryReader(new MemoryStream(data), Encoding.ASCII);

      Type type = NextBlock();

      if(type == Type.Table)
      {
        rows = ExtractShort();
        cols = ExtractShort();
      }
    }

    // **********************************************************************

    internal double ExtractDouble()
    {
      return br.ReadDouble();
    }

    // **********************************************************************

    internal int ExtractShort()
    {
      int s = br.ReadUInt16();
      pos += 2;
      return s;
    }

    // **********************************************************************

    public Type NextBlock()
    {
      pos = blockstart + blocksize + 4;

      if(pos >= data.Length)
        return Type.EOF;

      blockstart = pos;
      br.BaseStream.Seek(pos, SeekOrigin.Begin);
      int type = ExtractShort();
      blocksize = ExtractShort();

      switch(type)
      {
        case (short)Type.Blank:
          currentType = Type.Blank;
          blockitems = blocksize / 2;
          break;
        case (short)Type.Bool:
          currentType = Type.Bool;
          blockitems = blocksize / 2;
          break;
        case (short)Type.Error:
          currentType = Type.Error;
          blockitems = blocksize / 2;
          break;
        case (short)Type.Float:
          currentType = Type.Float;
          blockitems = blocksize / 8;
          break;
        case (short)Type.Int:
          currentType = Type.Int;
          blockitems = blocksize / 2;
          break;
        case (short)Type.Skip:
          currentType = Type.Skip;
          blockitems = blocksize / 2;
          break;
        case (short)Type.String:
          currentType = Type.String;
          blockitems = -1;
          break;
        case (short)Type.Table:
          currentType = Type.Table;
          blockitems = -1;
          break;
        default:
          currentType = Type.Unk;
          break;
      }

      return currentType;
    }

    // **********************************************************************

    bool Next()
    {
      if(pos >= blockstart + blocksize + 4)
        return false;

      switch(currentType)
      {
        case Type.Int:
        case Type.Skip:
        case Type.Blank:
        case Type.Error:
        case Type.Bool:
          intValue = ExtractShort();
          break;

        case Type.Float:
          floatValue = ExtractDouble();
          break;

        case Type.String:
          {
            int n = br.ReadByte();
            pos++;
            if(n == 0)
              strValue = "";
            else
            {
              strValue = Encoding.GetEncoding(codepage).GetString(data, pos, n);
              br.BaseStream.Seek(n, SeekOrigin.Current);
              pos += n;
            }
            break;
          }

        case Type.Table:
          intValue = ExtractShort();
          break;

        default:
          return false;
      }

      return true;
    }

    // **********************************************************************

    static void NextIndices(ref int i, ref int j, int n)
    {
      if(++j >= n)
      {
        j = 0;
        i++;
      }
    }

    // **********************************************************************

    public static Object[,] GetMatrix(byte[] tab)
    {
      XlTable xt = new XlTable(tab);
      Object[,] mtx = new Object[xt.Rows, xt.Columns];

      int nr = xt.Rows;
      int nc = xt.Columns;
      int i = 0, j = 0;
      Type bt;

      while((bt = xt.NextBlock()) != Type.EOF)
      {
        switch(bt)
        {
          case Type.Error:
            while(xt.Next())
            {
              mtx[i, j] = (Object)0;
              NextIndices(ref i, ref j, nc);
            }
            break;

          case Type.Float:

            for(int k = 0; k < xt.BlockItems; k++)
            {
              mtx[i, j] = (Object)xt.Stream.ReadDouble();
              if(++j >= nc)
              {
                j = 0;
                i++;
              }
            }
            break;

          case Type.Int:
          case Type.Bool:
            while(xt.Next())
            {
              mtx[i, j] = (Object)xt.IntValue;
              NextIndices(ref i, ref j, nc);
            }
            break;

          case Type.Skip:
          case Type.Blank:
            while(xt.Next())
            {
              int n = xt.IntValue;
              for(int q = 0; q < n; q++)
              {
                mtx[i, j] = (Object)0;
                NextIndices(ref i, ref j, nc);
              }
            }
            break;

          case Type.String:
            while(xt.Next())
            {
              mtx[i, j] = (Object)xt.StringValue;
              NextIndices(ref i, ref j, nc);
            }
            break;

          default:
            break;
        }
      }

      return mtx;
    }

  }
}
