// =======================================================================
//    DdeInput.cs (c) 2011 Nikolay Moroshkin, http://www.moroshkin.com/
// =======================================================================

using System;
using System.Collections.Generic;
using NDde.Server;
using XltDecoder;

namespace DdeInput
{
  // ************************************************************************
  // *                           DdeChannel class                           *
  // ************************************************************************

  public class DdeChannel
  {
    public delegate void OnUpdateDelegate(Object[,] table);

    OnUpdateDelegate onUpdate;
    Object[,] table;

    public bool Connected { get; set; }

    public DateTime Changed { get; set; }
    public DateTime DataReceived { get; set; }

    // **********************************************************************

    public OnUpdateDelegate OnUpdate { set { onUpdate = value; } }

    // **********************************************************************

    public Object[,] Table
    {
      get
      {
        return table;
      }
      set
      {
        table = value;

        if(onUpdate != null)
          onUpdate(table);
      }
    }

    // **********************************************************************

    public DdeChannel()
    {
      this.onUpdate = null;
      this.table = null;
      Connected = false;
      Changed = new DateTime();
      DataReceived = new DateTime();
    }

  }


  // ************************************************************************
  // *                          XltDdeServer class                          *
  // ************************************************************************

  public sealed class XltDdeServer : DdeServer
  {
    Dictionary<string, DdeChannel> channels;

    // **********************************************************************

    public Dictionary<string, DdeChannel> Channels { get { return channels; } }

    // **********************************************************************

    public XltDdeServer(string service, string[] topics)
      : base(service)
    {
      channels = new Dictionary<string, DdeChannel>();

      for(int i = 0; i < topics.GetLength(0); i++)
        channels.Add(topics[i], new DdeChannel());
    }

    // **********************************************************************

    protected override bool OnBeforeConnect(string topic)
    {
      return channels.ContainsKey(topic);
    }

    // **********************************************************************

    protected override void OnAfterConnect(DdeConversation conversation)
    {
      DdeChannel dc = channels[conversation.Topic];
      dc.Changed = DateTime.Now;
      dc.Connected = true;
    }

    // **********************************************************************

    protected override void OnDisconnect(DdeConversation conversation)
    {
      DdeChannel dc = channels[conversation.Topic];
      dc.Changed = DateTime.Now;
      dc.Connected = false;
    }

    // **********************************************************************

    protected override PokeResult OnPoke(DdeConversation conversation, string item, byte[] data, int format)
    {
      //if(format != xlTableFormat)
      //  return PokeResult.NotProcessed;

      DdeChannel dc = channels[conversation.Topic];
      dc.DataReceived = DateTime.Now;
      dc.Table = XlTable.GetMatrix(data);

      return PokeResult.Processed;
    }

    // **********************************************************************
  }

}
