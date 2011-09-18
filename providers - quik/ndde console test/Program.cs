using System;

class Program
{
    /// <summary>
    /// Програмка
    /// </summary>
    /// <param name="args"></param>
    static void Main(string[] args)
    {
        MyDDEServer server = new MyDDEServer("MyDdeServer");
        server.Register();
        Console.WriteLine("'MyDdeServer' started.\nHit Enter to Start Advise");
        Console.ReadLine();
        server.IsAdviseStarted = true;
        Console.WriteLine("Hit Enter to stop Server and Exit");
        Console.ReadLine();
    }
}


