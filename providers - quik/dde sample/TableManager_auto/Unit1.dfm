object Form1: TForm1
  Left = 214
  Top = 135
  Width = 870
  Height = 640
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsMDIForm
  Menu = MainMenu1
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object MainMenu1: TMainMenu
    Left = 424
    Top = 304
    object miTable: TMenuItem
      Caption = #1058#1072#1073#1083#1080#1094#1072
      object miTableCreate: TMenuItem
        Caption = #1057#1086#1079#1076#1072#1090#1100
        ShortCut = 16462
        OnClick = miTableCreateClick
      end
      object miTableDelete: TMenuItem
        Caption = #1059#1076#1072#1083#1080#1090#1100
        ShortCut = 16452
        OnClick = miTableDeleteClick
      end
      object miTableClear: TMenuItem
        Caption = #1054#1095#1080#1089#1090#1080#1090#1100
        ShortCut = 16451
        OnClick = miTableClearClick
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object miTabledeleteAll: TMenuItem
        Caption = #1059#1076#1072#1083#1080#1090#1100' '#1074#1089#1077
        ShortCut = 49220
        OnClick = miTabledeleteAllClick
      end
    end
  end
end
