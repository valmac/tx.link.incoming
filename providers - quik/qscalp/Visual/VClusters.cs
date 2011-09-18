// ======================================================================
//   VClusters.cs (c) 2011 Nikolay Moroshkin, http://www.moroshkin.com/
// ======================================================================

using System;
using System.Collections.Generic;
using System.Drawing;

namespace QScalp.Visual
{
  class VClusters : IDisposable
  {
    // **********************************************************************

    class Cell
    {
      int volume;

      public bool Updated { get; protected set; }
      public int Volume { get { Updated = false; return volume; } }
      public void Add(int v) { volume += v; Updated = true; }
      public Cell(int v) { volume = v; Updated = true; }
    }

    // **********************************************************************

    struct Cluster
    {
      public DateTime dt;
      public Dictionary<int, Cell> data;

      public Cluster(DateTime dt)
      {
        this.dt = dt;
        this.data = new Dictionary<int, Cell>();
      }
    }

    // **********************************************************************

    LinkedList<Cluster> clusters;

    VStock stock;

    Graphics bufGrfx;
    int height;

    bool invalidated;

    // **********************************************************************

    public Bitmap Bitmap { get; protected set; }
    public bool Updated { get; protected set; }

    // **********************************************************************

    public VClusters(VStock stock)
    {
      this.stock = stock;

      Bitmap = new Bitmap(1, 1);
      height = 1;

      bufGrfx = Graphics.FromImage(Bitmap);

      clusters = new LinkedList<Cluster>();
    }

    // **********************************************************************

    public int Height
    {
      get
      {
        return height;
      }
      set
      {
        height = (value > 0) ? value : 1;

        Bitmap = new Bitmap(Width, height);

        Graphics g = bufGrfx;
        bufGrfx = Graphics.FromImage(Bitmap);
        g.Dispose();

        Updated = true;
        invalidated = true;
      }
    }

    // **********************************************************************

    public void Clear()
    {
      lock(clusters)
        clusters.Clear();

      Updated = true;
      invalidated = true;
    }

    // **********************************************************************

    public void Add(int price, int volume, DateTime dt)
    {
      if(cfg.u.Clusters > 0)
      {
        lock(clusters)
        {
          if(clusters.Count == 0 || dt.Ticks - clusters.First.Value.dt.Ticks > cfg.u.ClusterInterval)
          {
            while(clusters.Count >= cfg.u.Clusters)
              clusters.RemoveLast();

            clusters.AddFirst(new Cluster(
              new DateTime(dt.Ticks / cfg.u.ClusterInterval * cfg.u.ClusterInterval)));

            invalidated = true;
          }

          if(clusters.First.Value.data.ContainsKey(price))
            clusters.First.Value.data[price].Add(volume);
          else
            clusters.First.Value.data.Add(price, new Cell(volume));
        }

        Updated = true;
      }
    }

    // **********************************************************************

    public static int Width { get { return (cfg.u.ClusterWidth + 2) * cfg.u.Clusters + 1; } }

    // **********************************************************************

    void PaintCluster(Cluster cluster, int x, bool force)
    {
      foreach(KeyValuePair<int, Cell> kvp in cluster.data)
        if((kvp.Value.Updated || force) && stock.QVisible(kvp.Key))
        {
          int v = kvp.Value.Volume;
          int y = stock.GetY(kvp.Key);

          bufGrfx.FillRectangle(cfg.BackBrush,
            x,
            y + 1,
            cfg.u.ClusterWidth,
            cfg.u.VQuoteHeight - 2);

          // --------------------------------------------------------------

          int vw;

          if(v > cfg.u.ClusterFillVolume1)
            vw = cfg.u.ClusterWidth - 2;
          else
            vw = v * (cfg.u.ClusterWidth - 2) / cfg.u.ClusterFillVolume1;

          bufGrfx.FillRectangle(cfg.ClusterVolumeFillBrush1,
            x + 1,
            y + cfg.u.VQuoteHeight / 2 - cfg.StringHeight / 2 + 1,
            vw,
            cfg.StringHeight - 2);

          if(v > cfg.u.ClusterFillVolume1)
          {
            if(v > cfg.u.ClusterFillVolume2)
              vw = cfg.u.ClusterWidth - 2;
            else
              vw = (v - cfg.u.ClusterFillVolume1) * (cfg.u.ClusterWidth - 2)
                / (cfg.u.ClusterFillVolume2 - cfg.u.ClusterFillVolume1);

            bufGrfx.FillRectangle(cfg.ClusterVolumeFillBrush2,
              x + 1,
              y + cfg.u.VQuoteHeight / 2 - cfg.StringHeight / 2 + 1,
              vw,
              cfg.StringHeight - 2);
          }

          // --------------------------------------------------------------

          bufGrfx.DrawRectangle(cfg.ClusterPen,
            x,
            y + 1,
            cfg.u.ClusterWidth - 1,
            cfg.u.VQuoteHeight - 3);

          // --------------------------------------------------------------

          bufGrfx.DrawString(v.ToString(), cfg.Font, cfg.ForeBrush,
            x + cfg.StringMargin,
            y + cfg.u.VQuoteHeight / 2 - cfg.StringHeight / 2);
        }

      // ----------------------------------------------------------------

      bufGrfx.FillRectangle(cfg.ClusterTimeBrush,
        x,
        height - cfg.StringHeight,
        cfg.u.ClusterWidth,
        cfg.StringHeight);

      bufGrfx.DrawString(cluster.dt.ToString("HH:mm:ss"), cfg.Font, cfg.ForeBrush,
        x - 1,
        height - cfg.StringHeight);
    }

    // **********************************************************************

    public void Paint()
    {
      int w = Width;

      if(invalidated)
      {
        invalidated = false;

        bufGrfx.FillRectangle(cfg.BackBrush, 0, 0, w - 1, height);
        bufGrfx.DrawLine(cfg.BlockLinePen, w - 1, 0, w - 1, height);

        // ------------------------------------------------------------------

        stock.PaintGrid(bufGrfx, cfg.u.Grid1Step, cfg.GridLine1Pen, w - 1, height);
        stock.PaintGrid(bufGrfx, cfg.u.Grid2Step, cfg.GridLine2Pen, w - 1, height);

        // ------------------------------------------------------------------

        lock(clusters)
        {
          while(clusters.Count > cfg.u.Clusters)
            clusters.RemoveLast();

          foreach(Cluster cluster in clusters)
          {
            w -= cfg.u.ClusterWidth + 2;
            PaintCluster(cluster, w, true);
          }
        }
      }
      else
      {
        lock(clusters)
          if(clusters.Count > 0)
            PaintCluster(clusters.First.Value, w - cfg.u.ClusterWidth - 2, false);
      }

      Updated = false;
    }

    // **********************************************************************

    public void Invalidate()
    {
      Updated = true;
      invalidated = true;
    }

    // **********************************************************************

    public void Dispose()
    {
      if(bufGrfx != null)
        bufGrfx.Dispose();
    }

    // **********************************************************************
  }
}
