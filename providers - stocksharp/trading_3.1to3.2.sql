USE Trading

alter TABLE [dbo].[Security] add [UnderlyingSecurityId] [nvarchar](256) NULL;
alter TABLE [dbo].[Security] add [Strike] [real] NULL;
alter TABLE [dbo].[Security] add [OptionType] [int] NULL;
alter TABLE [dbo].[Security] add [Volatility] [real] NULL;
alter TABLE [dbo].[Security] add [TheorPrice] [real] NULL;

update [Security]
set
	MinStepPrice = 1
where
	MinStepPrice is null or MinStepPrice = 0

ALTER TABLE [dbo].[Security] ALTER COLUMN [MinStepPrice] REAL NOT NULL;

GO

alter proc [dbo].[Security_UpdateById]
	@Id as nvarchar(256),
	@Name as nvarchar(512),
	@Code as nvarchar(256),
	@Class as nvarchar(256),
	@ShortName as nvarchar(256),
	@MinStepSize real,
	@MinStepPrice real,
	@MinLotSize real,
	@Decimals real,
	@OpenPrice real,
	@ClosePrice real,
	@LowPrice real,
	@HighPrice real,
	@State int,
	@Type int,
	@MinPrice real,
	@MaxPrice real,
	@MarginBuy real,
	@MarginSell real,
	@ExpiryDate datetime,
	@SettlementDate datetime,
	@ExtensionInfo nvarchar(max),
	@LastTradeId bigint,
	@LastTradeTime datetime2(7),
	@LastTradePrice real,
	@LastTradeVolume int,
	@LastTradeOrderDirection int,
	@BestBidPrice real,
	@BestBidVolume int,
	@BestBidOrderDirection int,
	@BestAskPrice real,
	@BestAskVolume int,
	@BestAskOrderDirection int,
	@Exchange nvarchar(64),
	@UnderlyingSecurityId nvarchar(256),
	@Strike real,
	@OptionType int,
	@Volatility real,
	@TheorPrice real
as
begin transaction

declare @ExtensionInfoXml xml
set @ExtensionInfoXml = convert(xml, @ExtensionInfo, 1)

declare @FinamMarketId bigint
declare @FinamSecurityId bigint
declare @Source nvarchar(128)
declare @TradeSource nvarchar(128)
declare @DepthSource nvarchar(128)
declare @IsSelected bit
declare @TradeCount int
declare @DepthCount int
declare @LastUpdateTime datetime

set @FinamMarketId = @ExtensionInfoXml.value(N'(/IDictionaryBeginObjectAndObjectEnd/KeyValuePairBeginObjectAndObjectEnd[key/Value = "FinamMarketId"]/value/Value)[1]', 'bigint')
set @FinamSecurityId = @ExtensionInfoXml.value(N'(/IDictionaryBeginObjectAndObjectEnd/KeyValuePairBeginObjectAndObjectEnd[key/Value = "FinamSecurityId"]/value/Value)[1]', 'bigint')
set @Source = @ExtensionInfoXml.value(N'(/IDictionaryBeginObjectAndObjectEnd/KeyValuePairBeginObjectAndObjectEnd[key/Value = "Source"]/value/Value)[1]', 'nvarchar(128)')
set @TradeSource = @ExtensionInfoXml.value(N'(/IDictionaryBeginObjectAndObjectEnd/KeyValuePairBeginObjectAndObjectEnd[key/Value = "TradeSource"]/value/Value)[1]', 'nvarchar(128)')
set @DepthSource = @ExtensionInfoXml.value(N'(/IDictionaryBeginObjectAndObjectEnd/KeyValuePairBeginObjectAndObjectEnd[key/Value = "DepthSource"]/value/Value)[1]', 'nvarchar(128)')
set @IsSelected = IsNull(@ExtensionInfoXml.value(N'(/IDictionaryBeginObjectAndObjectEnd/KeyValuePairBeginObjectAndObjectEnd[key/Value = "IsSelected"]/value/Value)[1]', 'bit'), 0)
set @TradeCount = IsNull(@ExtensionInfoXml.value(N'(/IDictionaryBeginObjectAndObjectEnd/KeyValuePairBeginObjectAndObjectEnd[key/Value = "TradeCount"]/value/Value)[1]', 'int'), 0)
set @DepthCount = IsNull(@ExtensionInfoXml.value(N'(/IDictionaryBeginObjectAndObjectEnd/KeyValuePairBeginObjectAndObjectEnd[key/Value = "DepthCount"]/value/Value)[1]', 'int'), 0)
--set @LastUpdateTime = @ExtensionInfoXml.value(N'(/IDictionaryBeginObjectAndObjectEnd/KeyValuePairBeginObjectAndObjectEnd[key/Value = "LastUpdateTime"]/value/Value)[1]', 'datetime')

begin try
	if (exists(select * from [Security] where Id = @Id))
	begin
		update [Security]
		set
			Name = @Name,
			Code = @Code,
			Class = @Class,
			ShortName = @ShortName,
			MinStepSize = @MinStepSize,
			MinStepPrice = @MinStepPrice,
			MinLotSize = @MinLotSize,
			Decimals = @Decimals,
			OpenPrice = @OpenPrice,
			ClosePrice = @ClosePrice,
			LowPrice = @LowPrice,
			HighPrice = @HighPrice,
			[State] = @State,
			[Type] = @Type,
			MinPrice = @MinPrice,
			MaxPrice = @MaxPrice,
			MarginBuy = @MarginBuy,
			MarginSell = @MarginSell,
			ExpiryDate = @ExpiryDate,
			SettlementDate = @SettlementDate,
			ExtensionInfo = @ExtensionInfo,
			LastTradeId = @LastTradeId,
			LastTradeTime = @LastTradeTime,
			LastTradePrice = @LastTradePrice,
			LastTradeVolume = @LastTradeVolume,
			LastTradeOrderDirection = @LastTradeOrderDirection,
			BestBidPrice = @BestBidPrice,
			BestBidVolume = @BestBidVolume,
			BestBidOrderDirection = @BestBidOrderDirection,
			BestAskPrice = @BestAskPrice,
			BestAskVolume = @BestAskVolume,
			BestAskOrderDirection = @BestAskOrderDirection,
			Exchange = @Exchange,
			UnderlyingSecurityId = @UnderlyingSecurityId,
			Strike = @Strike,
			OptionType = @OptionType,
			Volatility = @Volatility,
			TheorPrice = @TheorPrice
		where
			Id = @Id

		update HydraSecurityInfo
		set
			FinamMarketId = @FinamMarketId,
			FinamSecurityId = @FinamSecurityId,
			[Source] = @Source,
			TradeSource = @TradeSource,
			DepthSource = @DepthSource,
			IsSelected = @IsSelected,
			TradeCount = @TradeCount,
			DepthCount = @DepthCount,
			LastUpdateTime = @LastUpdateTime
		where
			[Security] = @Id
	end
	else
	begin
		insert into [Security]
			(Id, Name, Code, Class, ShortName, MinStepSize, MinStepPrice, MinLotSize, Decimals,
			OpenPrice, ClosePrice, LowPrice, HighPrice, [State], [Type], MinPrice, MaxPrice, MarginBuy, MarginSell, ExpiryDate,
			SettlementDate, ExtensionInfo, LastTradeId, LastTradeTime, LastTradePrice, LastTradeVolume, LastTradeOrderDirection,
			BestBidPrice, BestBidVolume, BestBidOrderDirection, BestAskPrice, BestAskVolume, BestAskOrderDirection, Exchange,
			OptionType, Strike,UnderlyingSecurityId, Volatility, TheorPrice)
		values
			(@Id, @Name, @Code, @Class, @ShortName, @MinStepSize, @MinStepPrice, @MinLotSize, @Decimals,
			@OpenPrice, @ClosePrice, @LowPrice, @HighPrice, @State, @Type, @MinPrice, @MaxPrice, @MarginBuy, @MarginSell, @ExpiryDate,
			@SettlementDate, @ExtensionInfo, @LastTradeId, @LastTradeTime, @LastTradePrice, @LastTradeVolume, @LastTradeOrderDirection,
			@BestBidPrice, @BestBidVolume, @BestBidOrderDirection, @BestAskPrice, @BestAskVolume, @BestAskOrderDirection, @Exchange,
			@OptionType, @Strike, @UnderlyingSecurityId, @Volatility, @TheorPrice)

		insert into HydraSecurityInfo
			([Security], TradeSource, DepthSource, [Source], IsSelected, TradeCount, DepthCount, FinamMarketId, FinamSecurityId, LastUpdateTime)
		values
           (@Id, @TradeSource, @DepthSource, @Source, @IsSelected, @TradeCount, @DepthCount, @FinamMarketId, @FinamSecurityId, @LastUpdateTime)
	end
end try
begin catch
	if @@TRANCOUNT > 0
		rollback transaction
		
	print 'Error Number: ' + str(error_number()) 
	print 'Line Number: ' + str(error_line())
	print error_message()
	
	exec usp_RethrowError
end catch

if @@TRANCOUNT > 0
	commit transaction
	
GO


UPDATE [Trading].[dbo].[Security]
   SET [UnderlyingSecurityId] = ''
      ,[Strike] = 0
      ,[Volatility] = 0
      ,[TheorPrice] = 0

GO

GO
ALTER TABLE [dbo].[Security] ALTER COLUMN [Strike] REAL NOT NULL;

ALTER TABLE [dbo].[Security] ALTER COLUMN [TheorPrice] REAL NOT NULL;

ALTER TABLE [dbo].[Security] ALTER COLUMN [Volatility] REAL NOT NULL;
