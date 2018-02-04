Imports System.Collections.ObjectModel
Imports System.ComponentModel

''' <summary>
''' An extended version of ObservableCollection that can be sorted
''' </summary>
''' <typeparam name="T"></typeparam>
''' <remarks></remarks>
Public Class ExtendedObservableCollection(Of T)
    Inherits ObservableCollection(Of T)

    Public Sub Sort()

        CType(Items, List(Of T)).Sort()
        OnCollectionChanged(New System.Collections.Specialized.NotifyCollectionChangedEventArgs(System.Collections.Specialized.NotifyCollectionChangedAction.Reset))

    End Sub

    Public Overrides Function ToString() As String
        Dim outString As String = ""
        Dim separator As String = ""
        For Each it As T In Me
            outString += separator & it.ToString()
            separator = ""","""
        Next
        Return outString
        'Return MyBase.ToString()
    End Function

    Protected Overrides Sub OnCollectionChanged(ByVal e As System.Collections.Specialized.NotifyCollectionChangedEventArgs)
        Unsubscribe(e.OldItems)
        Subscribe(e.NewItems)
        MyBase.OnCollectionChanged(e)
    End Sub

    Protected Overrides Sub ClearItems()
        For Each element As T In Me
            If TypeOf element Is INotifyPropertyChanged Then
                Dim el As INotifyPropertyChanged = CType(element, INotifyPropertyChanged)
                RemoveHandler el.PropertyChanged, AddressOf ContainedElementChanged
            End If
        Next
        MyBase.ClearItems()
    End Sub

    Private Sub Subscribe(ByRef iList As IList)
        If (iList IsNot Nothing) Then
            For Each element As T In iList
                Dim el As INotifyPropertyChanged = CType(element, INotifyPropertyChanged)
                AddHandler el.PropertyChanged, AddressOf ContainedElementChanged
            Next
        End If
    End Sub

    Private Sub Unsubscribe(ByRef iList As IList)
        If (iList IsNot Nothing) Then
            For Each element As T In iList
                Dim el As INotifyPropertyChanged = CType(element, INotifyPropertyChanged)
                RemoveHandler el.PropertyChanged, AddressOf ContainedElementChanged
            Next
        End If
    End Sub

    Private Sub ContainedElementChanged(ByVal sender As Object, ByVal args As PropertyChangedEventArgs)
        'OnPropertyChanged(args)
        OnCollectionChanged(New System.Collections.Specialized.NotifyCollectionChangedEventArgs(Specialized.NotifyCollectionChangedAction.Reset))
    End Sub

    Public Function ContainsStringDescriptor(ByVal descriptor As String) As Boolean
        For Each element As T In Me
            If element.ToString = descriptor Then
                Return True
            End If
        Next
        Return False
    End Function

    Public Function GetByStringDescriptor(ByVal descriptor As String) As T
        For Each element As T In Me
            If element.ToString = descriptor Then
                Return element
            End If
        Next
        Return Nothing
    End Function

End Class
