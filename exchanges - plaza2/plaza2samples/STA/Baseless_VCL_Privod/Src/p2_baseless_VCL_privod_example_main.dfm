object Form1: TForm1
  Left = 192
  Top = 111
  Width = 510
  Height = 614
  Caption = 'Скальперский привод'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefaultPosOnly
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label3: TLabel
    Left = 276
    Top = 40
    Width = 69
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'Код клиента :'
  end
  object Label4: TLabel
    Left = 276
    Top = 76
    Width = 32
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'Цена :'
  end
  object Label5: TLabel
    Left = 276
    Top = 100
    Width = 65
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'Количество :'
  end
  object LogListBox: TListBox
    Left = 4
    Top = 527
    Width = 493
    Height = 56
    Anchors = [akLeft, akRight, akBottom]
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Lucida Console'
    Font.Style = []
    ItemHeight = 11
    ParentFont = False
    TabOrder = 0
  end
  object ConnectButton: TButton
    Left = 275
    Top = 4
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Соединиться'
    TabOrder = 1
    OnClick = ConnectButtonClick
  end
  object DisconnectButton: TButton
    Left = 355
    Top = 4
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Разорвать'
    TabOrder = 2
    OnClick = DisconnectButtonClick
  end
  object InstrumentComboBox: TComboBox
    Left = 4
    Top = 4
    Width = 265
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    Sorted = True
    TabOrder = 3
    OnChange = InstrumentComboBoxChange
  end
  object OrderBookGrid: TStringGrid
    Tag = -1
    Left = 4
    Top = 28
    Width = 265
    Height = 496
    Anchors = [akLeft, akTop, akRight, akBottom]
    ColCount = 3
    DefaultRowHeight = 18
    FixedCols = 0
    RowCount = 1
    FixedRows = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goRowSelect]
    TabOrder = 4
    OnClick = OrderBookGridClick
  end
  object clientcodeedit: TEdit
    Left = 351
    Top = 36
    Width = 145
    Height = 21
    Anchors = [akTop, akRight]
    TabOrder = 5
    Text = '000'
  end
  object priceedit: TEdit
    Left = 351
    Top = 72
    Width = 145
    Height = 21
    Anchors = [akTop, akRight]
    TabOrder = 6
    Text = '1'
  end
  object qtyedit: TEdit
    Left = 351
    Top = 96
    Width = 145
    Height = 21
    Anchors = [akTop, akRight]
    TabOrder = 7
    Text = '1'
  end
  object buysellgroup: TRadioGroup
    Left = 276
    Top = 124
    Width = 221
    Height = 45
    Anchors = [akTop, akRight]
    Caption = ' Покупка/Продажа '
    Columns = 2
    ItemIndex = 0
    Items.Strings = (
      'Покупка'
      'Продажа')
    TabOrder = 8
  end
  object SendOrderButton: TButton
    Left = 276
    Top = 172
    Width = 221
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Отправить заявку'
    TabOrder = 9
    OnClick = SendOrderButtonClick
  end
  object positions: TTreeView
    Left = 276
    Top = 203
    Width = 221
    Height = 321
    Anchors = [akTop, akRight, akBottom]
    Indent = 19
    TabOrder = 10
    OnChange = positionsChange
  end
end
