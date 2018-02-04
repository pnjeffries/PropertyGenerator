Imports System.ComponentModel

Public Class CodeChunk
    Implements INotifyPropertyChanged
    Implements IComparable

    Private _SubChunks As ExtendedObservableCollection(Of CodeChunk)

    ''' <summary>
    ''' Properties and methods of this chunk (if the chunk is a class)
    ''' </summary>
    ''' <value></value>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Public Property SubChunks As ExtendedObservableCollection(Of CodeChunk)
        Get
            If _SubChunks Is Nothing Then
                _SubChunks = New ExtendedObservableCollection(Of CodeChunk)
            End If
            Return _SubChunks
        End Get
        Set(value As ExtendedObservableCollection(Of CodeChunk))
            _SubChunks = value
            NotifyPropertyChanged("SubChunks")
        End Set
    End Property

    Private _Parent As CodeChunk
    Public Property Parent As CodeChunk
        Get
            Return _Parent
        End Get
        Set(ByVal value As CodeChunk)
            _Parent = value
            NotifyPropertyChanged("Parent") 'Do not ever do this!
        End Set
    End Property

    Public ReadOnly Property HasChildren As Boolean
        Get
            If SubChunks.Count > 0 Then
                Return True
            Else
                Return False
            End If
        End Get
    End Property

    Private Property _Variables As ExtendedObservableCollection(Of CodeChunk)

    Public Property Variables As ExtendedObservableCollection(Of CodeChunk)
        Get
            If _Variables Is Nothing Then
                _Variables = New ExtendedObservableCollection(Of CodeChunk)
            End If
            Return _Variables
        End Get
        Set(ByVal value As ExtendedObservableCollection(Of CodeChunk))

        End Set
    End Property

    Public Property IsRoot As Boolean = False

    Public Sub New()

    End Sub

    Public Sub New(ByVal ChunkName As String)
        Name = ChunkName
    End Sub

    Public Sub New(ByVal root As Boolean)
        IsRoot = root
    End Sub

    Private _Name As String

    Public Property Name As String
        Get
            Return _Name
        End Get
        Set(ByVal value As String)
            _Name = value
            NotifyPropertyChanged("Name")
        End Set
    End Property

    Public Property IsExpanded As Boolean = False

    Public ReadOnly Property DisplayName As String
        Get
            If HasChildren Then
                Return Name
            ElseIf IsProperty Then
                Return Name
            Else
                Return Name & "()"
            End If
        End Get
    End Property

    Private _DefinedAt As Paragraph

    Public Property DefinedAt As Paragraph
        Get
            Return _DefinedAt
        End Get
        Set(value As Paragraph)
            _DefinedAt = value
        End Set
    End Property

    Public Function GetChunk(ByVal name As String) As CodeChunk
        Dim chunk As CodeChunk = SubChunks.GetByStringDescriptor(name)
        If chunk Is Nothing Then
            chunk = New CodeChunk(name)
            chunk.Parent = Me
            SubChunks.Add(chunk)
            SubChunks.Sort()
            NotifyPropertyChanged("HasChildren")
        End If
        Return chunk
    End Function

    Public Function GetVariable(ByVal name As String) As CodeChunk
        Dim var As CodeChunk = Variables.GetByStringDescriptor(name)
        If var Is Nothing Then
            var = New CodeChunk(name)
            var.Parent = Me
            Variables.Add(var)
            Variables.Sort()
        End If
        Return var
    End Function

    ''' <summary>
    ''' Is there a variable with this name in scope?
    ''' </summary>
    ''' <param name="name"></param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Public Function IsInScopeVariable(ByVal name As String) As CodeChunk
        Dim var As CodeChunk = Variables.GetByStringDescriptor(name)
        If var IsNot Nothing Then
            Return var
        ElseIf Parent IsNot Nothing Then
            Return Parent.IsInScopeVariable(name)
        Else
            Return Nothing
        End If
    End Function

    ''' <summary>
    ''' Notify any bound controls that a property of this object has been modified
    ''' </summary>
    ''' <param name="sender"></param>
    ''' <param name="e"></param>
    ''' <remarks></remarks>
    Public Event PropertyChanged(ByVal sender As Object, ByVal e As System.ComponentModel.PropertyChangedEventArgs) Implements System.ComponentModel.INotifyPropertyChanged.PropertyChanged

    ''' <summary>
    ''' Let it be known that a bindable property of this object has been modified
    ''' </summary>
    ''' <param name="p1"></param>
    ''' <remarks></remarks>
    Private Sub NotifyPropertyChanged(ByVal p1 As String)
        RaiseEvent PropertyChanged(Me, New PropertyChangedEventArgs(p1))
    End Sub

    Public Function CompareTo(obj As Object) As Integer Implements System.IComparable.CompareTo
        Dim other As CodeChunk = DirectCast(obj, CodeChunk)
        Return Name.CompareTo(other.Name)
    End Function

    Public Overrides Function ToString() As String
        Return Name
    End Function

    Public Function GetTopLevelChunk() As CodeChunk
        If Parent IsNot Nothing AndAlso Not Parent.IsRoot Then
            Return Parent.GetTopLevelChunk()
        Else
            Return Me
        End If
    End Function

    Private _Type As CodeChunk = Nothing
    Public Property Type As CodeChunk
        Get
            Return _Type
        End Get
        Set(ByVal value As CodeChunk)
            _Type = value
        End Set
    End Property

    Private _IsProperty As Boolean = False

    Public Property IsProperty As Boolean
        Get
            Return _IsProperty
        End Get
        Set(ByVal value As Boolean)
            _IsProperty = value
            NotifyPropertyChanged("IsProperty")
            NotifyPropertyChanged("DisplayName")
        End Set
    End Property

    Private _AttachedComment As String

    Public Property AttachedComment As String
        Get
            Return _AttachedComment
        End Get
        Set(value As String)
            _AttachedComment = value
            NotifyPropertyChanged("AttachedComment")
        End Set
    End Property

    Public Function GetSubChunkStartMatch(ByVal prompt As String) As CodeChunk

        prompt = prompt.ToLower
        For Each chunk As CodeChunk In SubChunks
            If chunk.Name.Length >= prompt.Length AndAlso chunk.Name.Substring(0, prompt.Length).ToLower = prompt Then
                Return chunk
            End If
        Next

        Return Nothing

    End Function

    Public Function GetVariableStartMatch(ByVal prompt As String) As CodeChunk

        prompt = prompt.ToLower
        For Each chunk As CodeChunk In Variables
            If chunk.Name.Length >= prompt.Length AndAlso chunk.Name.Substring(0, prompt.Length).ToLower = prompt Then
                Return chunk
            End If
        Next

        Return Nothing

    End Function

End Class
