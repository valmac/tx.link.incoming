object Form1: TForm1
  Left = 192
  Top = 103
  Width = 455
  Height = 411
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefaultPosOnly
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object LogListBox: TListBox
    Left = 4
    Top = 3
    Width = 360
    Height = 377
    Anchors = [akLeft, akTop, akRight, akBottom]
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
    Left = 368
    Top = 4
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Connect'
    TabOrder = 1
    OnClick = ConnectButtonClick
  end
  object DisconnectButton: TButton
    Left = 368
    Top = 32
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Disconnect'
    TabOrder = 2
    OnClick = DisconnectButtonClick
  end
end
