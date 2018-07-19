Option Strict On
Option Explicit On

Imports System.Drawing.Drawing2D
Imports System.Xml
Imports SlimDX.DirectInput
Imports SlimDX.RawInput

Public Class MainGame
    Inherits System.Windows.Forms.Form

    Private c() As CircleF
    Private p() As Polygon
    Private eyePt As MovableObject = New MovableObject(0, 0, 1)
    Private scaleFtIn As Integer = 10 'feet per inch
    Private qtRoot As QuadTreeBranch = New QuadTreeBranch(New RectangleF(0, 0, 5280, 5280))
    Private vrgn As VisibleRegion
    Private wol As ArrayList
    Private bluePen As New Pen(Color.Blue, 2)
    Private redPen As New Pen(Color.Red, 1)
    Private scaleX As Single
    Private scaleY As Single
    Private lightGrayBrush As New SolidBrush(Color.LightGray)
    Private blackBrush As New SolidBrush(Color.Black)
    Private g As Graphics
    Private fr As Integer
    Private kstate As KeyboardState
    Private lastTime As DateTime = DateTime.Now
    Private elapsed As TimeSpan
    Private baseSpd As Single
    Private baseAnglularSpd As Single
    Private movementRect As RectangleF

    Private directInput As DirectInput
    Private keyboard As Keyboard

    Const baspd As Single = 60 * 2 / 3 * Math.PI 'rad/min


#Region " Windows Form Designer generated code "

    Public Sub New()
        MyBase.New()

        'This call is required by the Windows Form Designer.
        InitializeComponent()

        'Add any initialization after the InitializeComponent() call

        'Init DirectInput
        directInput = New DirectInput
        keyboard = New Keyboard(directInput)
        keyboard.SetCooperativeLevel(Me, CooperativeLevel.Nonexclusive Or CooperativeLevel.Background)
        keyboard.Acquire()

        InitWorld()
    End Sub

    'Form overrides dispose to clean up the component list.
    Protected Overloads Overrides Sub Dispose(ByVal disposing As Boolean)
        If disposing Then
            If Not (components Is Nothing) Then
                components.Dispose()
            End If
        End If
        MyBase.Dispose(disposing)
    End Sub

    'Required by the Windows Form Designer
    Private components As System.ComponentModel.IContainer

    'NOTE: The following procedure is required by the Windows Form Designer
    'It can be modified using the Windows Form Designer.  
    'Do not modify it using the code editor.
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.SuspendLayout()
        '
        'MainGame
        '
        Me.AutoScaleBaseSize = New System.Drawing.Size(5, 13)
        Me.BackColor = System.Drawing.Color.Black
        Me.ClientSize = New System.Drawing.Size(621, 607)
        Me.Name = "MainGame"
        Me.Text = "Form1"
        Me.ResumeLayout(False)

    End Sub

#End Region

    Private Sub Form1_Paint(ByVal sender As Object, ByVal e As System.Windows.Forms.PaintEventArgs) Handles MyBase.Paint
        MainLoop()
        DrawIt(e.Graphics)
        Me.Invalidate()
    End Sub

    Private Sub InitWorld()
        Me.ResizeRedraw = True
        Me.SetStyle(ControlStyles.AllPaintingInWmPaint, True)
        Me.SetStyle(ControlStyles.Opaque, True)
        Me.SetStyle(ControlStyles.UserPaint, True)
        Me.SetStyle(ControlStyles.DoubleBuffer, True)

        Dim xmlr As New XmlTextReader("test.xml")
        xmlr.DtdProcessing = DtdProcessing.Ignore
        Dim i, j, k As Integer
        Dim wo As WorldObject

        While xmlr.Read()
            If xmlr.LocalName = "circle" Then
                ReDim Preserve c(i)
                xmlr.MoveToAttribute("cx")
                c(i).center.X = CSng(xmlr.Value)
                xmlr.MoveToAttribute("cy")
                c(i).center.Y = CSng(xmlr.Value)
                xmlr.MoveToAttribute("r")
                c(i).radius = CSng(xmlr.Value)

                If xmlr.MoveToAttribute("fill") Then
                    c(i).fill = System.Drawing.ColorTranslator.FromHtml(xmlr.Value)
                Else
                    c(i).fill = System.Drawing.Color.Black
                End If


                wo = New WorldObject(c(i))
                qtRoot.AddObject(wo)

                i = i + 1
            ElseIf xmlr.LocalName = "polygon" Then
                xmlr.MoveToAttribute("points")

                Dim newpoly As Polygon = New Polygon(CStr(xmlr.Value))

                If xmlr.MoveToAttribute("fill") Then
                    newpoly.fill = System.Drawing.ColorTranslator.FromHtml(xmlr.Value)
                Else
                    newpoly.fill = System.Drawing.Color.Black
                End If

                'if polygon is concave it must be decomposed into multiple convex polygons
                Dim plst As List(Of Polygon) = newpoly.Decompose

                For Each poly As Polygon In plst
                    ReDim Preserve p(k)
                    p(k) = poly
                    wo = New WorldObject(p(k))
                    qtRoot.AddObject(wo)
                    k = k + 1
                Next

            End If
        End While

        'add a bunch of random objects to the world
        'approx 180 trees per acre = 11520 trees per sq mile
        '6" to 5'6" in diameter
        ReDim Preserve c(i + 11520)
        For j = i To i + 11520
            c(j).center.X = Rnd() * 5280
            c(j).center.Y = Rnd() * 5280
            c(j).radius = CSng(0.25 + Rnd() * 2.5)
            c(j).fill = Color.BurlyWood
            wo = New WorldObject(c(j), WOSubtypeEnum.WOST_TREE)
            qtRoot.AddObject(wo)
        Next

        g = Me.CreateGraphics
        bluePen.Width = 1

        scaleX = g.DpiX / scaleFtIn
        scaleY = g.DpiY / scaleFtIn
        bluePen.ScaleTransform(1 / scaleX, 1 / scaleY)

        TransformGraphic(g)
        vrgn = New VisibleRegion(g.ClipBounds)
        wol = qtRoot.GetCollisionList(Nothing, g.ClipBounds, g)
        For i = 0 To wol.Count - 1
            CType(wol(i), WorldObject).alreadyAdded = False
        Next

    End Sub

    Private Sub MainLoop()

        'read inputs
        eyePt.Halt()
        baseSpd = 300 'ft/min
        baseAnglularSpd = baspd

        kstate = keyboard.GetCurrentState

        'move the pieces
        elapsed = DateTime.Now.Subtract(lastTime)
        If elapsed.Ticks <> 0 Then fr = CInt(TimeSpan.TicksPerSecond / elapsed.Ticks)
        Me.Text = Now.Second & " " & fr & " FPS " & wol.Count & " " & eyePt.X & ":" & eyePt.Y
        lastTime = DateTime.Now

        If kstate.IsPressed(Key.LeftShift) Then
            'hustle x2
            baseSpd = baseSpd + baseSpd
            baseAnglularSpd = baseAnglularSpd / 2
        End If
        If kstate.IsPressed(Key.LeftControl) Then
            'run x3
            baseSpd = baseSpd + baseSpd
            baseAnglularSpd = baseAnglularSpd / 2
        End If
        If kstate.IsPressed(Key.LeftAlt) Then
            'sprint x4
            baseAnglularSpd = baseAnglularSpd / 2
            baseSpd = baseSpd + baseSpd
        End If

        If kstate.IsPressed(Key.LeftArrow) Then
            eyePt.angularVelocity = -baseAnglularSpd
        End If
        If kstate.IsPressed(Key.RightArrow) Then
            eyePt.angularVelocity = baseAnglularSpd
        End If

        If kstate.IsPressed(Key.A) Then
            'left
            eyePt.StrafeLeft(Math.Min(baseSpd, 300))
        End If
        If kstate.IsPressed(Key.D) Then
            'right
            eyePt.StrafeRight(Math.Min(baseSpd, 300))
        End If
        If kstate.IsPressed(Key.W) Or kstate.IsPressed(Key.UpArrow) Then
            'forward
            eyePt.ForwardBackward(baseSpd)
        End If
        If kstate.IsPressed(Key.S) Or kstate.IsPressed(Key.DownArrow) Then
            'backward
            eyePt.ForwardBackward(-Math.Min(baseSpd, 600))
        End If
        If kstate.IsPressed(Key.Escape) Then
            Application.Exit()
        End If

        movementRect = eyePt.Move(elapsed, qtRoot, g)


    End Sub



    Private Sub TransformGraphic(ByRef g As Graphics)
        '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
        'set up the matrix so that the viewpoint is always at the center
        'scale so that X feet in the world equals 1 inch on the screen
        'and the world rotates about the eyept
        ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
        g.ScaleTransform(scaleX, scaleY, MatrixOrder.Append)
        g.TranslateTransform((g.VisibleClipBounds.Width / 2 - eyePt.X) * scaleX, (g.VisibleClipBounds.Height / 2 - eyePt.Y) * scaleY, MatrixOrder.Append)
        Dim m As New Matrix()
        Dim pts(0) As PointF
        pts(0) = eyePt.c.center
        g.TransformPoints(CoordinateSpace.Device, CoordinateSpace.World, pts)
        m.RotateAt(-CSng(Math.Atan2(eyePt.facing.Y, eyePt.facing.X) * OneEightyOverPI) - 90, pts(0), MatrixOrder.Append)
        g.MultiplyTransform(m, MatrixOrder.Append)
    End Sub

    Private Sub DrawIt(ByVal g As Graphics)
        Dim i As Integer

        TransformGraphic(g)

        g.Clear(System.Drawing.Color.DarkGray)

        Dim vrect As RectangleF = g.VisibleClipBounds
        wol = qtRoot.GetCollisionList(Nothing, g.ClipBounds, g)
        For i = 0 To wol.Count - 1
            CType(wol(i), WorldObject).alreadyAdded = False
        Next
        Dim wo As WorldObject

        vrgn = New VisibleRegion(vrect)
        For i = 0 To wol.Count - 1
            wo = CType(wol.Item(i), WorldObject)
            If wo.type = WorldObjects.WOTypeEnum.WOT_CIRCLE Then
                vrgn.SubtractRegionBehindCircle(eyePt.c.center, wo.c, Geometry.PathTypeEnum.PT_CHORD)
            ElseIf wo.type = WorldObjects.WOTypeEnum.WOT_POLYGON Then
                vrgn.SubtractRegionBehindCircle(eyePt.c.center, wo.p, Geometry.PathTypeEnum.PT_CHORD)
            Else
                Err.Raise(999)
            End If
        Next i

        g.FillRegion(lightGrayBrush, vrgn.vrgn)

        'this is the users moving avatar, line indicates facing
        g.DrawEllipse(bluePen, eyePt.BoundingRectangleF)
        g.DrawLine(bluePen, eyePt.X, eyePt.Y, eyePt.X + 1 * eyePt.facing.X, eyePt.Y + 1 * eyePt.facing.Y)

        Dim br As System.Drawing.SolidBrush = blackBrush

        For i = 0 To wol.Count - 1
            wo = CType(wol.Item(i), WorldObject)
            If vrgn.IsCircleVisible(wo.c, g) Then
                If wo.type = WorldObjects.WOTypeEnum.WOT_CIRCLE Then
                    br = New SolidBrush(wo.c.fill)
                    g.FillEllipse(br, wo.c.CircumscribedRectangleF)
                    If wo.subtype = WOSubtypeEnum.WOST_TREE Then
                        DrawTree(g, wo)
                    End If
                ElseIf wo.type = WorldObjects.WOTypeEnum.WOT_POLYGON Then
                    br = New SolidBrush(wo.p.fill)
                    g.FillPolygon(br, wo.p.Points)
                    g.DrawEllipse(bluePen, wo.p.CircumscribedCircleF.CircumscribedRectangleF)
                Else

                End If
            End If
        Next i

        'draw grid lines to aid in navigation while debugging
        For i = 0 To 5280 Step scaleFtIn


            g.DrawLine(bluePen, 0, i, 5280, i)
            g.DrawLine(bluePen, i, 0, i, 5280)
        Next
    End Sub

    ''' <summary>
    ''' Draw a tree
    ''' </summary>
    ''' <param name="g"></param>
    ''' <param name="wo"></param>
    ''' <remarks>http://www.ajol.info/index.php/bajopas/article/viewFile/58808/47133</remarks>
    Private Sub DrawTree(ByVal g As Graphics, ByVal wo As WorldObject)
        Rnd(-1)
        Randomize(wo.c.X * wo.c.Y * wo.c.radius)

        Dim br As New SolidBrush(wo.c.fill)
        Dim p As New Pen(br)
        p.Width = wo.c.radius

        Dim crwn As Single = CSng((35.02 * wo.c.radius * 2 + 0.881) / 2)

        Dim cnt As Integer = CInt(Math.Floor(5 * Rnd())) + 3

        Dim angle As Double = 360 / (cnt + 1)

        For a As Double = 0 To 360 Step angle
            Dim aa As Double = a + CInt(Math.Floor(21 * Rnd())) - 10

            aa = aa * Math.PI / 180

            Dim cc As Single = crwn + CInt(Math.Floor((2 * crwn / 4 + 1) * Rnd())) - crwn / 4


            'DrawBranch(g, p, wo.c.center, aa, cc, 1)

        Next

    End Sub

    Sub DrawBranch(ByVal g As Graphics, ByVal p As Pen, ByVal start As PointF, ByVal angle As Double, ByVal length As Single, ByVal level As Integer)

        Dim sz As New SizeF((length / 3) * CSng(Math.Cos(angle)), (length / 3) * CSng(Math.Sin(angle)))

        Dim pt1 As New PointF(start.X, start.Y)
        For l As Integer = 0 To 2
            Dim pt2 As PointF = PointF.Add(pt1, sz)
            g.DrawLine(p, pt1, pt2)
            If level < 3 Then
                p.Width = p.Width / 3
                DrawBranch(g, p, pt2, angle + Math.PI / 2, length / 3, level + 1)
                DrawBranch(g, p, pt2, angle - Math.PI / 2, length / 3, level + 1)
                p.Width = p.Width * 3
            End If
            pt1 = pt2
        Next



    End Sub

End Class
