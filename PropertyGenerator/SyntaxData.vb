Public Class SyntaxData

    Private _delimiters As HashSet(Of Char)
    Private _keywords As HashSet(Of String)
    Private _stringLiteralDelimiters As List(Of Char)
    Private _inlineCommentDelimiters As List(Of String)
    Private _blockCommentStartDelimiters As List(Of String)
    Private _blockCommentEndDelimiters As List(Of String)
    Private _indentDelimiters As List(Of Char)
    Private _outdentDelimiters As List(Of Char)
    Private _functionKeywords As List(Of String)
    Private _prototypeKeywords As List(Of String)
    Private _accessors As List(Of String)
    Private _selfReferenceKeywords As List(Of String)
    Private _variableDeclarationKeywords As List(Of String)
    Private _assignmentOperators As List(Of String)

    Public Property FileFilter As String = "JavaScript files (*.js)|*.js|All files (*.*)|*.*"

    Public Sub LoadJavaScript()

        FileFilter = "JavaScript files (*.js)|*.js|All files (*.*)|*.*"

        'Populate Delimiters
        _delimiters = New HashSet(Of Char)
        _delimiters.Add(" ")
        _delimiters.Add(".")
        _delimiters.Add(";")
        _delimiters.Add(",")
        _delimiters.Add("(")
        _delimiters.Add(")")
        _delimiters.Add("{")
        _delimiters.Add("}")
        _delimiters.Add("[")
        _delimiters.Add("]")
        _delimiters.Add("+")
        _delimiters.Add("-")
        _delimiters.Add("/")
        _delimiters.Add("*")
        _delimiters.Add("=")
        _delimiters.Add("%")
        _delimiters.Add(">")
        _delimiters.Add("<")
        _delimiters.Add("&")
        _delimiters.Add("|")
        _delimiters.Add("^")
        _delimiters.Add("~")
        _delimiters.Add("!")

        'Populate keywords
        _keywords = New HashSet(Of String)
        _keywords.Add("break")
        _keywords.Add("catch")
        _keywords.Add("const")
        _keywords.Add("continue")
        _keywords.Add("delete")
        _keywords.Add("do")
        _keywords.Add("else")
        _keywords.Add("export")
        _keywords.Add("for")
        _keywords.Add("function")
        _keywords.Add("if")
        _keywords.Add("import")
        _keywords.Add("in")
        _keywords.Add("instanceof")
        _keywords.Add("label")
        _keywords.Add("let")
        _keywords.Add("new")
        _keywords.Add("prototype")
        _keywords.Add("return")
        _keywords.Add("switch")
        _keywords.Add("this")
        _keywords.Add("throw")
        _keywords.Add("try")
        _keywords.Add("typeof")
        _keywords.Add("var")
        _keywords.Add("void")
        _keywords.Add("while")
        _keywords.Add("with")
        _keywords.Add("yield")

        'Populate string delimiters
        _stringLiteralDelimiters = New List(Of Char)
        _stringLiteralDelimiters.Add(Chr(34))

        'Comment delimiters
        _inlineCommentDelimiters = New List(Of String)
        _inlineCommentDelimiters.Add("//")
        _blockCommentStartDelimiters = New List(Of String)
        _blockCommentStartDelimiters.Add("/*")
        _blockCommentEndDelimiters = New List(Of String)
        _blockCommentEndDelimiters.Add("*/")

        'Indenting/outdenting:
        _indentDelimiters = New List(Of Char)
        _indentDelimiters.Add("{")
        _outdentDelimiters = New List(Of Char)
        _outdentDelimiters.Add("}")

        'function declaration
        _functionKeywords = New List(Of String)
        _functionKeywords.Add("function")

        'prototype keywords - pretty much just for javascript parsing, I think...
        _prototypeKeywords = New List(Of String)
        _prototypeKeywords.Add("prototype")

        'accessors
        _accessors = New List(Of String)
        _accessors.Add(".")

        'self-reference:
        _selfReferenceKeywords = New List(Of String)
        _selfReferenceKeywords.Add("this")

        'variable declaration:
        _variableDeclarationKeywords = New List(Of String)
        _variableDeclarationKeywords.Add("var")

        'assignment operators:
        _assignmentOperators = New List(Of String)
        _assignmentOperators.Add("=")

    End Sub

    Public Sub LoadJava()
        FileFilter = "Java files (*.js)|*.js|All files (*.*)|*.*"

        'Populate Delimiters
        _delimiters = New HashSet(Of Char)
        _delimiters.Add(" ")
        _delimiters.Add(".")
        _delimiters.Add(";")
        _delimiters.Add(":")
        _delimiters.Add(",")
        _delimiters.Add("(")
        _delimiters.Add(")")
        _delimiters.Add("{")
        _delimiters.Add("}")
        _delimiters.Add("[")
        _delimiters.Add("]")
        _delimiters.Add("+")
        _delimiters.Add("-")
        _delimiters.Add("/")
        _delimiters.Add("*")
        _delimiters.Add("=")
        _delimiters.Add("%")
        _delimiters.Add(">")
        _delimiters.Add("<")
        _delimiters.Add("&")
        _delimiters.Add("|")
        _delimiters.Add("^")
        _delimiters.Add("~")
        _delimiters.Add("!")
        _delimiters.Add("?")

        'Populate keywords
        _keywords = New HashSet(Of String)
        _keywords.Add("abstract")
        _keywords.Add("assert")
        _keywords.Add("boolean")
        _keywords.Add("break")
        _keywords.Add("byte")
        _keywords.Add("case")
        _keywords.Add("catch")
        _keywords.Add("class")
        _keywords.Add("const")
        _keywords.Add("continue")
        _keywords.Add("default")
        _keywords.Add("do")
        _keywords.Add("double")
        _keywords.Add("else")
        _keywords.Add("enum")
        _keywords.Add("extends")
        _keywords.Add("final")
        _keywords.Add("finally")
        _keywords.Add("float")
        _keywords.Add("for")
        _keywords.Add("if")
        _keywords.Add("implements")
        _keywords.Add("import")
        _keywords.Add("instanceof")
        _keywords.Add("int")
        _keywords.Add("interface")
        _keywords.Add("long")
        _keywords.Add("native")
        _keywords.Add("new")
        _keywords.Add("null")
        _keywords.Add("package")
        _keywords.Add("private")
        _keywords.Add("protected")
        _keywords.Add("public")
        _keywords.Add("return")
        _keywords.Add("short")
        _keywords.Add("static")
        _keywords.Add("strictfp")
        _keywords.Add("super")
        _keywords.Add("switch")
        _keywords.Add("synchronised")
        _keywords.Add("this")
        _keywords.Add("throw")
        _keywords.Add("throws")
        _keywords.Add("transient")
        _keywords.Add("try")
        _keywords.Add("void")
        _keywords.Add("volatile")
        _keywords.Add("while")

        'Populate string delimiters
        _stringLiteralDelimiters = New List(Of Char)
        _stringLiteralDelimiters.Add(Chr(34))

        'Comment delimiters
        _inlineCommentDelimiters = New List(Of String)
        _inlineCommentDelimiters.Add("//")
        _blockCommentStartDelimiters = New List(Of String)
        _blockCommentStartDelimiters.Add("/*")
        _blockCommentStartDelimiters.Add("/**")
        _blockCommentEndDelimiters = New List(Of String)
        _blockCommentEndDelimiters.Add("*/")
        _blockCommentEndDelimiters.Add("**/")

        'Indenting/outdenting:
        _indentDelimiters = New List(Of Char)
        _indentDelimiters.Add("{")
        _outdentDelimiters = New List(Of Char)
        _outdentDelimiters.Add("}")

        'function declaration
        _functionKeywords = New List(Of String)
        '_functionKeywords.Add("function")
        _functionKeywords.Add("class")

        'prototype keywords - pretty much just for javascript parsing, I think...
        _prototypeKeywords = New List(Of String)
        '_prototypeKeywords.Add("class")
        '_prototypeKeywords.Add("prototype")

        'accessors
        _accessors = New List(Of String)
        _accessors.Add(".")

        'self-reference:
        _selfReferenceKeywords = New List(Of String)
        _selfReferenceKeywords.Add("this")

        'variable declaration:
        _variableDeclarationKeywords = New List(Of String)
        '_variableDeclarationKeywords.Add("var")

        'assignment operators:
        _assignmentOperators = New List(Of String)
        _assignmentOperators.Add("=")
    End Sub

    Public Sub LoadCG()

        FileFilter = "CG files (*.cg)|*.cg|All files (*.*)|*.*"

        _delimiters = New HashSet(Of Char)
        _delimiters.Add(" ")
        _delimiters.Add(".")
        _delimiters.Add(":")
        _delimiters.Add(";")
        _delimiters.Add(",")
        _delimiters.Add("(")
        _delimiters.Add(")")
        _delimiters.Add("{")
        _delimiters.Add("}")
        _delimiters.Add("[")
        _delimiters.Add("]")
        _delimiters.Add("+")
        _delimiters.Add("-")
        _delimiters.Add("/")
        _delimiters.Add("*")
        _delimiters.Add("=")
        _delimiters.Add("%")
        _delimiters.Add(">")
        _delimiters.Add("<")
        _delimiters.Add("&")
        _delimiters.Add("|")
        _delimiters.Add("^")
        _delimiters.Add("~")
        _delimiters.Add("!")

        'Populate keywords
        _keywords = New HashSet(Of String)
        _keywords.Add("asm*")
        _keywords.Add("asm_fragment")
        _keywords.Add("auto")
        _keywords.Add("bool")
        _keywords.Add("break")
        _keywords.Add("case")
        _keywords.Add("catch")
        _keywords.Add("char")
        _keywords.Add("class")
        _keywords.Add("column_major")
        _keywords.Add("compile")
        _keywords.Add("const")
        _keywords.Add("const_cast")
        _keywords.Add("continue")
        _keywords.Add("decl*")
        _keywords.Add("default")
        _keywords.Add("delete")
        _keywords.Add("discard")
        _keywords.Add("do")
        _keywords.Add("double")
        _keywords.Add("dword*")
        _keywords.Add("dynamic_cast")
        _keywords.Add("else")
        _keywords.Add("emit")
        _keywords.Add("enum")
        _keywords.Add("explicit")
        _keywords.Add("extern")
        _keywords.Add("FALSE")
        _keywords.Add("fixed")
        _keywords.Add("float")
        _keywords.Add("float*")
        _keywords.Add("for")
        _keywords.Add("friend")
        _keywords.Add("get")
        _keywords.Add("goto")
        _keywords.Add("half")
        _keywords.Add("if")
        _keywords.Add("in")
        _keywords.Add("inline")
        _keywords.Add("inout")
        _keywords.Add("int")
        _keywords.Add("interface")
        _keywords.Add("long")
        _keywords.Add("matrix*")
        _keywords.Add("mutable")
        _keywords.Add("namespace")
        _keywords.Add("new")
        _keywords.Add("operator")
        _keywords.Add("out")
        _keywords.Add("packed")
        _keywords.Add("pass*")
        _keywords.Add("pixelfragment*")
        _keywords.Add("pixelshader*")
        _keywords.Add("private")
        _keywords.Add("protected")
        _keywords.Add("public")
        _keywords.Add("register")
        _keywords.Add("reinterpret_cast")
        _keywords.Add("return")
        _keywords.Add("row_major")
        _keywords.Add("sampler")
        _keywords.Add("sampler_state")
        _keywords.Add("sampler1D")
        _keywords.Add("sampler2D")
        _keywords.Add("sampler3D")
        _keywords.Add("samplerCUBE")
        _keywords.Add("shared")
        _keywords.Add("short")
        _keywords.Add("signed")
        _keywords.Add("sizeof")
        _keywords.Add("static")
        _keywords.Add("static_cast")
        _keywords.Add("string*")
        _keywords.Add("struct")
        _keywords.Add("switch")
        _keywords.Add("technique*")
        _keywords.Add("template")
        _keywords.Add("texture*")
        _keywords.Add("texture1D")
        _keywords.Add("texture2D")
        _keywords.Add("texture3D")
        _keywords.Add("textureCUBE")
        _keywords.Add("textureRECT")
        _keywords.Add("this")
        _keywords.Add("throw")
        _keywords.Add("TRUE")
        _keywords.Add("try")
        _keywords.Add("typedef")
        _keywords.Add("typeid")
        _keywords.Add("typename")
        _keywords.Add("uniform")
        _keywords.Add("union")
        _keywords.Add("unsigned")
        _keywords.Add("using")
        _keywords.Add("vector*")
        _keywords.Add("vertexFragment*")
        _keywords.Add("vertexshader*")
        _keywords.Add("virtual")
        _keywords.Add("void")
        _keywords.Add("volatile")
        _keywords.Add("while")
        'vector and matrix datatypes:
        For i As Integer = 1 To 4
            _keywords.Add("float" & i)
            _keywords.Add("int" & i)
            _keywords.Add("half" & i)
            _keywords.Add("double" & i)
            _keywords.Add("fixed" & i)
            For j As Integer = 1 To 4
                _keywords.Add("float" & i & "x" & j)
                _keywords.Add("int" & i & "x" & j)
                _keywords.Add("half" & i & "x" & j)
                _keywords.Add("double" & i & "x" & j)
                _keywords.Add("fixed" & i & "x" & j)
            Next
        Next
        'binding semantics: (put somewhere else?)
        For i As Integer = 0 To 15
            _keywords.Add("ATTR" & i)
        Next
        _keywords.Add("BCOL0")
        _keywords.Add("BCOL1")
        _keywords.Add("BINORMAL")
        _keywords.Add("BLENDINDICES")
        _keywords.Add("BLENDWEIGHT")
        For i As Integer = 0 To 5
            _keywords.Add("CLP" & i)
        Next
        _keywords.Add("COLOR")
        _keywords.Add("COL0")
        _keywords.Add("COLOR0")
        _keywords.Add("COL1")
        _keywords.Add("COLOR1")
        _keywords.Add("DEPTH")
        _keywords.Add("DIFFUSE")
        _keywords.Add("FOG")
        _keywords.Add("FOGC")
        _keywords.Add("FOGCOORD")
        _keywords.Add("HPOS")
        _keywords.Add("NORMAL")
        _keywords.Add("POSITION")
        _keywords.Add("PSIZ")
        _keywords.Add("PSIZE")
        _keywords.Add("SPECULAR")
        _keywords.Add("TANGENT")
        _keywords.Add("TEXCOORD")
        For i As Integer = 0 To 7
            _keywords.Add("TEXCOORD" & i)
            _keywords.Add("TEX" & i)
        Next
        _keywords.Add("TESSFACTOR")


        'Populate string delimiters
        _stringLiteralDelimiters = New List(Of Char)
        _stringLiteralDelimiters.Add(Chr(34))

        'Comment delimiters
        _inlineCommentDelimiters = New List(Of String)
        _inlineCommentDelimiters.Add("//")
        _blockCommentStartDelimiters = New List(Of String)
        _blockCommentStartDelimiters.Add("/*")
        _blockCommentEndDelimiters = New List(Of String)
        _blockCommentEndDelimiters.Add("*/")

        'Indenting/outdenting:
        _indentDelimiters = New List(Of Char)
        _indentDelimiters.Add("{")
        _outdentDelimiters = New List(Of Char)
        _outdentDelimiters.Add("}")

        'function declaration
        _functionKeywords = New List(Of String)
        '_functionKeywords.Add("function")
        _functionKeywords.Add("struct")

        'prototype keywords - pretty much just for javascript parsing, I think...
        _prototypeKeywords = New List(Of String)
        '_prototypeKeywords.Add("prototype")

        'accessors
        _accessors = New List(Of String)
        _accessors.Add(".")

        'self-reference:
        _selfReferenceKeywords = New List(Of String)
        _selfReferenceKeywords.Add("this")

        'variable declaration:
        _variableDeclarationKeywords = New List(Of String)
        _variableDeclarationKeywords.Add("var")

        'assignment operators:
        _assignmentOperators = New List(Of String)
        _assignmentOperators.Add("=")

    End Sub

    Public Sub New()

        'LoadCG()
        'LoadJavaScript()
        LoadJava()

    End Sub

    Public Function IsStringLiteralDelimiter(ByVal c As Char) As Boolean
        Return _stringLiteralDelimiters.Contains(c)
    End Function

    Public Function IsDelimiter(ByVal c As Char) As Boolean
        Return _delimiters.Contains(c)
    End Function

    Public Function IsKeyWord(ByVal word As String) As Boolean
        Return _keywords.Contains(word)
    End Function

    Public Function IsInlineCommentDelimiter(ByVal word As String) As Boolean
        Return _inlineCommentDelimiters.Contains(word) OrElse _blockCommentStartDelimiters.Contains(word)
    End Function

    Public Function IsBlockCommentStartDelimiter(ByVal word As String) As Boolean
        Return _blockCommentStartDelimiters.Contains(word)
    End Function

    Public Function IsBlockCommentEndDelimiter(ByVal word As String) As Boolean
        Return _blockCommentEndDelimiters.Contains(word)
    End Function

    Public Function IsDecimalSeparator(ByVal separator As String) As Boolean
        If separator = Globalization.CultureInfo.CurrentCulture.NumberFormat.NumberDecimalSeparator Then
            Return True
        Else
            Return False
        End If
    End Function

    Public Function IsIndentDelimiter(ByVal c As Char)
        Return _indentDelimiters.Contains(c)
    End Function

    Public Function IsOutdentDelimiter(ByVal c As Char)
        Return _outdentDelimiters.Contains(c)
    End Function

    Public Function IsNumeric(ByVal word As String) As Boolean
        Return Double.TryParse(word, 0)
    End Function

    Public Function IsFunctionKeyword(ByVal word As String) As Boolean
        Return _functionKeywords.Contains(word)
    End Function

    Public Function IsPrototypeKeyword(ByVal word As String) As Boolean
        Return _prototypeKeywords.Contains(word)
    End Function

    Public Function IsAccessor(ByVal symbol As String) As Boolean
        Return _accessors.Contains(symbol)
    End Function

    Public Function DefaultAccessor() As String
        If _accessors IsNot Nothing AndAlso _accessors.Count > 0 Then
            Return _accessors(0)
        Else
            Return "."
        End If
    End Function

    Public Function IsSelfReference(ByVal name As String) As Boolean
        Return _selfReferenceKeywords.Contains(name)
    End Function

    Public Function IsVariableDeclarationKeyword(ByVal name As String) As Boolean
        Return _variableDeclarationKeywords.Contains(name)
    End Function

    Public Function IsAssignmentOperator(ByVal symbol As String) As Boolean
        Return _assignmentOperators.Contains(symbol)
    End Function

End Class
