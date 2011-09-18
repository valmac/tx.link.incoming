object Form1: TForm1
  Left = 192
  Top = 107
  Width = 427
  Height = 412
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 48
    Width = 91
    Height = 13
    Caption = 'Connection params'
  end
  object Label2: TLabel
    Left = 16
    Top = 176
    Width = 79
    Height = 13
    Caption = 'Transaction text:'
  end
  object CBShowMsg: TCheckBox
    Left = 16
    Top = 16
    Width = 105
    Height = 17
    Caption = 'Show Diagnostic'
    Checked = True
    State = cbChecked
    TabOrder = 0
  end
  object QUIKTerminalPathEdit: TEdit
    Left = 115
    Top = 40
    Width = 273
    Height = 21
    TabOrder = 1
  end
  object Button1: TButton
    Left = 16
    Top = 80
    Width = 110
    Height = 20
    Caption = 'Connect'
    TabOrder = 2
    OnClick = OnConnect
  end
  object Button2: TButton
    Left = 152
    Top = 80
    Width = 110
    Height = 20
    Caption = 'Disconnect'
    TabOrder = 3
    OnClick = onDisconnectClick
  end
  object Button4: TButton
    Left = 16
    Top = 112
    Width = 110
    Height = 20
    Caption = 'Is DLL Connected'
    TabOrder = 4
    OnClick = OnIsDllConnected
  end
  object Button5: TButton
    Left = 280
    Top = 112
    Width = 110
    Height = 20
    Caption = 'Is Quik Connected'
    TabOrder = 5
    OnClick = onIsQuikConnectedClick
  end
  object Button6: TButton
    Left = 16
    Top = 144
    Width = 110
    Height = 20
    Caption = 'Async Trans'
    TabOrder = 6
    OnClick = OnAsyncClick
  end
  object Button7: TButton
    Left = 280
    Top = 144
    Width = 110
    Height = 20
    Caption = 'Sync Trans'
    TabOrder = 7
    OnClick = OnSyncClick
  end
  object Memo1: TMemo
    Left = 16
    Top = 192
    Width = 377
    Height = 177
    Lines.Strings = (
      '')
    ScrollBars = ssBoth
    TabOrder = 8
  end
end
