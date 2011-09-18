namespace SampleRealTimeTesting
{
	using System;

	using StockSharp.Algo;
	using StockSharp.Algo.Candles;
	using StockSharp.Algo.Strategies;
	using StockSharp.BusinessEntities;

	class SmaStrategy : TimeFrameStrategy
	{
		private readonly CandleManager _candleManager;
		private bool _isShortLessThenLong;

		private DateTime _nextTime;

		public SmaStrategy(CandleManager candleManager, Ma longMa, Ma shortMa, TimeSpan timeFrame)
			: base(timeFrame)
		{
			_candleManager = candleManager;

			this.LongMa = longMa;
			this.ShortMa = shortMa;
		}

		public Ma LongMa { get; private set; }
		public Ma ShortMa { get; private set; }

		protected override void OnStarting()
		{
			// запоминаем текущее положение относительно друг друга
			_isShortLessThenLong = this.ShortMa.Value < this.LongMa.Value;

			// вычисляем время окончания текущей пятиминутки
			_nextTime = base.TimeFrame.GetCandleBounds(base.Trader).Max;

			base.OnStarting();
		}

		protected override StrategyProcessResults OnProcess()
		{
			// если наша стратегия в процессе остановки
			if (base.ProcessState == StrategyProcessStates.Stopping)
			{
				// отменяем активные заявки
				base.CancelActiveOrders();

				// так как все активные заявки гарантированно были отменены, то возвращаем StrategyProcessResults.Stop
				return StrategyProcessResults.Stop;
			}

			// событие обработки торговой стратегии вызвалось впервый раз, что раньше, чем окончания текущей 5-минутки.
			if (base.Trader.MarketTime < _nextTime)
			{
				// возвращаем StrategyProcessResults.Continue, так как наш алгоритм еще не закончил свою работу, а просто ожидает следующего вызова.
				return StrategyProcessResults.Continue;
			}

			// получаем сформированную свечку
			var candle = _candleManager.GetTimeFrameCandle(base.Security, base.TimeFrame, _nextTime - base.TimeFrame);

			// если свечки не существует (не было ни одной сделке в тайм-фрейме), то ждем окончания следующей свечки.
			if (candle == null)
			{
				// если прошло больше 10 секунд с момента окончания свечки, а она так и не появилась,
				// значит сделок в прошедшей 5-минутке не было, и переходим на следующую свечку
				if ((base.Trader.MarketTime - _nextTime) > TimeSpan.FromSeconds(10))
					_nextTime += base.TimeFrame;

				return StrategyProcessResults.Continue;
			}

			_nextTime += base.TimeFrame;

			// добавляем новую свечку
			this.LongMa.Add(candle);
			this.ShortMa.Add(candle);

			// вычисляем новое положение относительно друг друга
			var isShortLessThenLong = this.ShortMa.Value < this.LongMa.Value;

			// если произошло пересечение
			if (_isShortLessThenLong != isShortLessThenLong)
			{
				// если короткая меньше чем длинная, то продажа, иначе, покупка.
				var direction = isShortLessThenLong ? OrderDirections.Sell : OrderDirections.Buy;

				// создаем заявку
				var order = base.CreateOrder(direction, base.Security.GetMarketPrice(direction), base.Volume);

				// регистрируем заявку (обычным способом - лимитированной заявкой)
				// base.RegisterOrder(order);

				// регистрируем заявку (через котирование)
				var strategy = new MarketQuotingStrategy(order, new Unit(), new Unit());
				base.ChildStrategies.Add(strategy);

				// запоминаем текущее положение относительно друг друга
				_isShortLessThenLong = isShortLessThenLong;
			}

			return StrategyProcessResults.Continue;
		}
	}
}