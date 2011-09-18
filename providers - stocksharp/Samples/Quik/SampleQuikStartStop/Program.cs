namespace SampleStartStopQuik
{
	using System;

	using StockSharp.Quik;

	class Program
	{
		static void Main()
		{
			Console.Write("Введите путь к директории с Quik: ");
			var path = Console.ReadLine();

			Console.Write("Введите логин: ");
			var login = Console.ReadLine();

			Console.Write("Введите пароль: ");
			var password = Console.ReadLine();

			try
			{
				var terminal = QuikTerminal.Get(path);

				if (!terminal.IsLaunched)
				{
					Console.WriteLine("Запускается Quik...");

					terminal.Launch();
					Console.WriteLine("Quik запущен.");
				}
				else
					Console.WriteLine("Quik найден.");

				if (!terminal.IsConnected)
				{
					terminal.Login(login, password);
					Console.WriteLine("Авторизация произведена.");
				}

				Console.WriteLine("Нажмите Enter для выключения Quik...");
				Console.ReadLine();

				terminal.Logout();
				Console.WriteLine("Quik отключен от торговли.");

				terminal.Exit();
				Console.WriteLine("Quik выключен.");
			}
			catch (Exception ex)
			{
				Console.WriteLine(ex);
			}
		}
	}
}