Option Strict On
Option Explicit On
Imports Microsoft.Xna.Framework
Imports VelcroPhysics.Shared
Imports VelcroPhysics.Tools.Triangulation.TriangulationBase

Public Module Geometry
    Public OneEightyOverPI As Single = 180 / Math.PI
    Public PIOver2 As Single = Math.PI / 2
    Public TwoPI As Single = 2 * Math.PI

    Public Enum PathTypeEnum
        PT_CHORD = 0
        PT_FRONT_ARC
        PT_BACK_ARC
    End Enum

    Public Interface IGeometricArea

        ReadOnly Property CircumscribedRectangleF() As Drawing.RectangleF
        Function TangentPointPlus(ByVal eyePt As PointF) As PointF
        Function TangentPointMinus(ByVal eyePt As PointF) As PointF
        Sub AddSectionToPath(ByVal pth As Drawing2D.GraphicsPath, ByVal sect As PathTypeEnum, _
            ByVal pnt1 As PointF, ByVal pnt2 As PointF, ByVal pa As PlaneAngle)


    End Interface

    'Represents the distance between two points on the Cartesian plane
    Public Structure DistanceF
        Implements IComparable

        Public value As Single

        Public Function CompareTo(ByVal d As Object) As Integer Implements IComparable.CompareTo
            Return value.CompareTo(d)
        End Function

        Public Sub New(ByVal e1 As PointF, ByVal e2 As PointF)
            Me.value = CSng(Math.Sqrt((e1.X - e2.X) ^ 2 + (e1.Y - e2.Y) ^ 2))
        End Sub

        Public Sub New(ByVal v As Single)
            Me.value = v
        End Sub

        Shared Function op_LessThan(ByVal d1 As DistanceF, ByVal d2 As DistanceF) As Boolean
            Return d1.value < d2.value
        End Function

        Shared Function op_LessThan(ByVal d1 As DistanceF, ByVal d2 As Single) As Boolean
            Return d1.value < d2
        End Function

        Shared Function op_LessThan(ByVal d1 As Single, ByVal d2 As DistanceF) As Boolean
            Return d1 < d2.value
        End Function

    End Structure

    'Represents a circle with a given center point and radius
    Public Structure CircleF
        Implements IGeometricArea
        Public center As PointF
        Public radius As Single
        Public fill As System.Drawing.Color

        Public Sub New(ByVal c As PointF, ByVal r As Single)
            Me.center = c
            Me.radius = r
            Me.fill = System.Drawing.Color.Black
        End Sub

        Public Sub New(ByVal x As Single, ByVal y As Single, ByVal r As Single)
            Me.center.X = x
            Me.center.Y = y
            Me.radius = r
            Me.fill = System.Drawing.Color.Black
        End Sub

        Public Property X() As Single
            Get
                X = center.X
            End Get
            Set(ByVal Value As Single)
                center.X = Value
            End Set
        End Property

        Public Property Y() As Single
            Get
                Y = center.Y
            End Get
            Set(ByVal Value As Single)
                center.Y = Value
            End Set
        End Property

        'get the rectangle that curcumscribes the circle
        Public ReadOnly Property CircumscribedRectangleF() As Drawing.RectangleF _
            Implements IGeometricArea.CircumscribedRectangleF
            Get
                CircumscribedRectangleF = New RectangleF(center.X - radius, center.Y - radius, 2 * radius, 2 * radius)
            End Get
        End Property

        'Returns the point on the circle where the first tangent line
        'from the eye point intersects
        Public Function TangentPointPlus(ByVal eyePt As PointF) As PointF _
            Implements IGeometricArea.TangentPointPlus
            Dim ret As New PointF(0, 0)
            Dim hypot As Double
            Dim d1 As New DistanceF(eyePt, Me.center)
            Dim pa1 As New PlaneAngle(eyePt, Me)

            Dim a1 As Double = pa1.anglePlus

            hypot = Math.Sqrt(d1.value ^ 2 - Me.radius ^ 2)
            ret.X = eyePt.X + CSng(Math.Cos(a1) * hypot)
            ret.Y = eyePt.Y + CSng(Math.Sin(a1) * hypot)
            Return ret
        End Function

        'Returns the point on the circle where the second tangent line
        'from the eye point intersects
        Public Function TangentPointMinus(ByVal eyePt As PointF) As PointF _
            Implements IGeometricArea.TangentPointMinus
            Dim ret As New PointF(0, 0)
            Dim hypot As Double
            Dim d1 As New DistanceF(eyePt, Me.center)
            Dim pa1 As New PlaneAngle(eyePt, Me)

            Dim a1 As Double = pa1.angleMinus

            hypot = Math.Sqrt(d1.value ^ 2 - Me.radius ^ 2)
            ret.X = eyePt.X + CSng(Math.Cos(a1) * hypot)
            ret.Y = eyePt.Y + CSng(Math.Sin(a1) * hypot)
            Return ret
        End Function

        Sub AddSectionToPath(ByVal pth As Drawing2D.GraphicsPath, ByVal pathtype As PathTypeEnum, _
            ByVal pnt1 As PointF, ByVal pnt2 As PointF, ByVal pa As PlaneAngle) _
            Implements IGeometricArea.AddSectionToPath

            If pathtype = PathTypeEnum.PT_CHORD Then
                'chord connecting the two tangent points on circle
                pth.AddLine(pnt1, pnt2)
            ElseIf pathtype = PathTypeEnum.PT_BACK_ARC Then
                'arc across back of circle connecting the two tangent points 
                pth.AddArc(Me.CircumscribedRectangleF, CSng(OneEightyOverPI * Math.Atan2(pnt1.Y - Me.center.Y, pnt1.X - Me.center.X)), -CSng(180 + 2 * OneEightyOverPI * pa.angleSubtended))
            ElseIf pathtype = PathTypeEnum.PT_FRONT_ARC Then
                'arc across front of circle connecting the two tangent points 
                pth.AddArc(Me.CircumscribedRectangleF, CSng(OneEightyOverPI * Math.Atan2(pnt1.Y - Me.center.Y, pnt1.X - Me.center.X)), CSng(180 - 2 * OneEightyOverPI * pa.angleSubtended))
            Else
                'chord connecting the two tangent points on circle
                pth.AddLine(pnt1, pnt2)
            End If
        End Sub

    End Structure

    'Represents the plane angle subtended by a circle on the Cartesian plane
    Public Structure PlaneAngle
        Public angleSubtended As Single
        Public directionAngle As Single
        Public anglePlus As Single
        Public angleMinus As Single

        Public Sub New(ByVal eyePt As PointF, ByVal area As IGeometricArea)
            Dim c As CircleF
            Dim p As Polygon

            If TypeName(area) = "Polygon" Then
                p = CType(area, Polygon)
                InitForPolygon(eyePt, p)
            ElseIf TypeName(area) = "CircleF" Then
                c = CType(area, CircleF)
                InitForCircleF(eyePt, c)
            Else
            End If
        End Sub

        Public Sub New(ByVal eyePt As PointF, ByVal c As CircleF)
            InitForCircleF(eyePt, c)
        End Sub

        Private Sub InitForCircleF(ByVal eyePt As PointF, ByVal c As CircleF)
            Dim d As New DistanceF(eyePt, c.center)

            angleSubtended = CSng(Math.Asin(c.radius / d.value))
            directionAngle = CSng(Math.Atan2(c.center.Y - eyePt.Y, c.center.X - eyePt.X))

            anglePlus = directionAngle + angleSubtended
            angleMinus = directionAngle - angleSubtended
        End Sub

        Public Sub New(ByVal eyePt As PointF, ByVal p As Polygon)
            InitForPolygon(eyePt, p)
        End Sub

        Private Sub InitForPolygon(ByVal eyePt As PointF, ByVal p As Polygon)
            Dim tpp As PointF = p.TangentPointPlus(eyePt)
            Dim tpm As PointF = p.TangentPointMinus(eyePt)

            anglePlus = CSng(Math.Atan2(tpp.Y - eyePt.Y, tpp.X - eyePt.X))
            angleMinus = CSng(Math.Atan2(tpm.Y - eyePt.Y, tpm.X - eyePt.X))

            'find shortest angle between two angles
            angleSubtended = anglePlus - angleMinus
            While angleSubtended > Math.PI
                angleSubtended = angleSubtended - TwoPI
            End While
            While angleSubtended < -Math.PI
                angleSubtended = angleSubtended + TwoPI
            End While

            'midway angle between the two angles
            directionAngle = angleMinus + angleSubtended / 2

        End Sub

    End Structure

    'region representing the visible part of area map
    Public Class VisibleRegion
        Public vrgn As Drawing.Region
        Private offset As Single
        Private clipRectF As RectangleF


        Public Sub New(ByVal rect As Drawing.Rectangle)
            clipRectF = New RectangleF(rect.X, rect.Y, rect.Width, rect.Height)
            offset = rect.Width + rect.Height
            vrgn = New Region(rect)
        End Sub

        Public Sub New(ByVal rect As Drawing.RectangleF)
            clipRectF = New RectangleF(rect.X, rect.Y, rect.Width, rect.Height)
            offset = CLng(rect.Width + rect.Height)
            vrgn = New Region(rect)
        End Sub

        Public Sub SubtractRegionBehindCircle(ByVal eyePt As PointF, ByVal c As CircleF, ByVal pathtype As PathTypeEnum)
            If clipRectF.IntersectsWith(c.CircumscribedRectangleF) Then
                Dim pa As New PlaneAngle(eyePt, c)
                Dim pth As New Drawing2D.GraphicsPath(Drawing.Drawing2D.FillMode.Alternate)

                Dim pnt1 As PointF = c.TangentPointPlus(eyePt)
                Dim pnt2 As PointF = c.TangentPointMinus(eyePt)

                Dim pnt3 As PointF
                pnt3.X = pnt1.X + CSng(offset * Math.Cos(pa.anglePlus))
                pnt3.Y = pnt1.Y + CSng(offset * Math.Sin(pa.anglePlus))

                Dim pnt4 As PointF
                pnt4.X = pnt2.X + CSng(offset * Math.Cos(pa.angleMinus))
                pnt4.Y = pnt2.Y + CSng(offset * Math.Sin(pa.angleMinus))

                pth.AddLine(pnt3, pnt1)
                c.AddSectionToPath(pth, pathtype, pnt1, pnt2, pa)
                pth.AddLine(pnt2, pnt4)

                vrgn.Exclude(pth)
            End If
        End Sub

        Public Sub SubtractRegionBehindCircle(ByVal eyePt As PointF, ByVal c As IGeometricArea, ByVal pathtype As PathTypeEnum)
            If clipRectF.IntersectsWith(c.CircumscribedRectangleF) Then
                Dim pa As New PlaneAngle(eyePt, c)
                Dim pth As New Drawing2D.GraphicsPath(Drawing.Drawing2D.FillMode.Alternate)

                Dim pnt1 As PointF = c.TangentPointPlus(eyePt)
                Dim pnt2 As PointF = c.TangentPointMinus(eyePt)

                Dim pnt3 As PointF
                pnt3.X = pnt1.X + CSng(offset * Math.Cos(pa.anglePlus))
                pnt3.Y = pnt1.Y + CSng(offset * Math.Sin(pa.anglePlus))

                Dim pnt4 As PointF
                pnt4.X = pnt2.X + CSng(offset * Math.Cos(pa.angleMinus))
                pnt4.Y = pnt2.Y + CSng(offset * Math.Sin(pa.angleMinus))

                Dim pnt5 As PointF
                pnt5.X = pnt3.X + CSng(offset * Math.Cos(pa.directionAngle))
                pnt5.Y = pnt3.Y + CSng(offset * Math.Sin(pa.directionAngle))

                Dim pnt6 As PointF
                pnt6.X = pnt4.X + CSng(offset * Math.Cos(pa.directionAngle))
                pnt6.Y = pnt4.Y + CSng(offset * Math.Sin(pa.directionAngle))

                pth.AddLine(pnt5, pnt3)
                pth.AddLine(pnt3, pnt1)
                c.AddSectionToPath(pth, pathtype, pnt1, pnt2, pa)
                pth.AddLine(pnt2, pnt4)
                pth.AddLine(pnt4, pnt6)

                vrgn.Exclude(pth)
            End If
        End Sub

        Public Function IsCircleVisible(ByVal c As CircleF, ByVal g As Graphics) As Boolean
            IsCircleVisible = vrgn.IsVisible(c.CircumscribedRectangleF, g)
        End Function

        Public Function IsCircleVisible(ByVal c As CircleF) As Boolean
            IsCircleVisible = vrgn.IsVisible(c.CircumscribedRectangleF)
        End Function

    End Class

    Public Structure Polygon
        Implements IGeometricArea
        Public Points() As PointF
        Public Count As Integer
        Private index As Integer
        Public fill As System.Drawing.Color

        ''' <summary>
        ''' Convert this polygon into a Farseer Vertices object
        ''' </summary>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Private Function GetFarseerVertices() As Vertices
            Dim vert As New Vertices()
            For Each p As PointF In Points
                vert.Add(New Vector2(p.X, p.Y))
            Next
            Return vert
        End Function

        ''' <summary>
        ''' Convert a Farseer Vertices object into a Polygon
        ''' </summary>
        ''' <param name="vert"></param>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Private Shared Function ConvertFromFarseerVertices(ByVal vert As Vertices) As Polygon
            Dim ret As New Polygon
            For Each v As Vector2 In vert
                ret.AddPoint(New PointF(v.X, v.Y))
            Next
            Return ret
        End Function

        ''' <summary>
        ''' Use the Farseer decomposition classes to decompose convex into concave polygons; also forces polygons to be wound CCW
        ''' </summary>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Function Decompose() As List(Of Polygon)
            Dim ret As New List(Of Polygon)
            Dim vert As Vertices = GetFarseerVertices()

            vert.ForceCounterClockWise()

            Dim verts As List(Of Vertices) = Triangulate.ConvexPartition(vert, TriangulationAlgorithm.Earclip)

            For Each v As Vertices In verts
                Dim badpoly As PolygonError = v.CheckPolygon
                If Not badpoly.HasFlag(PolygonError.NoError) Then Throw New Exception("Invalid polygon detected.")
                Dim p As Polygon = Polygon.ConvertFromFarseerVertices(v)
                p.fill = Me.fill
                ret.Add(p)
            Next

            Return ret
        End Function



        Public Sub New(ByVal pts() As PointF)
            Points = pts
            Count = UBound(Points) - LBound(Points) + 1
            index = LBound(Points)
            fill = System.Drawing.Color.Black
        End Sub

        Public Function GotoPoint(ByVal pt As PointF) As Integer
            Return GotoPoint(pt, 0)
        End Function

        Public Function GotoPoint(ByVal pt As PointF, ByVal tol As Single) As Integer
            Dim i As Integer

            If Count > 0 Then

                For i = LBound(Points) To UBound(Points)
                    If Math.Abs(New DistanceF(Points(i), pt).value) <= tol Then
                        index = i
                        Exit For
                    Else
                        index = -1
                    End If
                Next
            Else
                index = -1
            End If

            Return index
        End Function

        Public Function CurrentPoint() As PointF
            If Count > 0 Then
                Return Points(index)
            Else
                Return Nothing
            End If
        End Function

        Public Function NextPoint() As PointF
            If Count > 0 Then
                index = index + 1
                If index > UBound(Points) Then index = LBound(Points)
                Return CurrentPoint()
            Else
                Return Nothing
            End If
        End Function

        Public Function PreviousPoint() As PointF
            If Count > 0 Then
                index = index - 1
                If index < LBound(Points) Then index = UBound(Points)
                Return CurrentPoint()
            Else
                Return Nothing
            End If
        End Function

        Public Sub New(ByVal str As String)
            'the string conforms to the syntax for the SVG points attribute
            Dim coords() As String
            Dim coord As String
            Dim newpt As PointF
            Dim i As Integer
            Dim delims(4) As Char
            delims(0) = ","c
            delims(1) = " "c
            delims(2) = CChar(vbTab)
            delims(3) = CChar(vbLf)
            delims(4) = CChar(vbCr)

            coords = CStr(str).Split(delims)

            For Each coord In coords
                If Len(coord) > 0 Then
                    If i Mod 2 = 0 Then
                        newpt.X = CSng(coord)
                    Else
                        newpt.Y = CSng(coord)
                        AddPoint(New PointF(newpt.X, newpt.Y))
                    End If
                    i = i + 1
                End If
            Next coord

            index = LBound(Points)
        End Sub

        Public Sub AddPoint(ByVal pt As PointF)
            ReDim Preserve Points(Count)
            Points(Count) = pt
            Count = Count + 1
        End Sub

        'get the rectangle that curcumscribes the polygon
        Public ReadOnly Property CircumscribedRectangleF() As Drawing.RectangleF _
            Implements IGeometricArea.CircumscribedRectangleF
            Get
                Dim pth As New Drawing2D.GraphicsPath()
                pth.AddLines(Points)
                Return pth.GetBounds
            End Get
        End Property

        Public ReadOnly Property CircumscribedCircleF() As CircleF
            Get
                Return Me.fastBall(Points, Count)
            End Get
        End Property

        'Returns the point on the polygon where the first tangent line
        'from the eye point intersects
        Public Function TangentPointPlus(ByVal eyePt As PointF) As PointF _
            Implements IGeometricArea.TangentPointPlus
            Dim ret As New PointF(0, 0)
            Dim rtan, ltan As Integer

            tangent_PointPoly(eyePt, Count, Points, rtan, ltan)

            'TODO: Need to check angles of rtan and ltan to make sure correct one is returned, the one with greater angle should be returned here

            ret = Points(ltan)
            Return ret
        End Function

        'Returns the point on the polygon where the second tangent line
        'from the eye point intersects
        Public Function TangentPointMinus(ByVal eyePt As PointF) As PointF _
            Implements IGeometricArea.TangentPointMinus
            Dim ret As New PointF(0, 0)
            Dim rtan, ltan As Integer

            tangent_PointPoly(eyePt, Count, Points, rtan, ltan)

            'TODO: Need to check angles of rtan and ltan to make sure correct one is returned, the one with lesser angle should be returned here

            ret = Points(rtan)
            Return ret
        End Function

        '===================================================================
        'isLeft(): test if a point is Left|On|Right of an infinite line.
        'Input:  three points P0, P1, and P2
        'Return:    >0 for P2 left of the line through P0 and P1
        '           =0 for P2 on the line
        '           <0 for P2 right of the line
        'See: the January 2001 Algorithm on Area of Triangles
        Private Function isLeft(ByVal P0 As PointF, ByVal P1 As PointF, ByVal P2 As PointF) As Single
            Return (P1.X - P0.X) * (P2.Y - P0.Y) - (P2.X - P0.X) * (P1.Y - P0.Y)
        End Function

        'tests for polygon vertex ordering relative to a fixed point P
        Private Function above(ByVal P As PointF, ByVal Vi As PointF, ByVal Vj As PointF) As Boolean
            Return (isLeft(P, Vi, Vj) > 0) 'true if Vi is above Vj
        End Function

        Private Function below(ByVal P As PointF, ByVal Vi As PointF, ByVal Vj As PointF) As Boolean
            Return (isLeft(P, Vi, Vj) < 0) 'true if Vi is below Vj
        End Function

        '===================================================================
        'tangent_PointPoly(): find any polygon's exterior tangents
        'Input: P = a 2D point (exterior to the polygon)
        '       n = number of polygon vertices
        '       V = array of vertices for any 2D polygon with V[n]!=V[0]
        'Output:    rtan = index of rightmost tangent point V[*rtan]
        '           ltan = index of leftmost tangent point V[*ltan]

        Private Sub tangent_PointPoly(ByVal P As PointF, ByVal n As Integer, ByVal V() As PointF, ByRef rtan As Integer, ByRef ltan As Integer)
            Dim eprev, enext As Single        'V[i] previous and next edge turn direction
            Dim i As Integer
            rtan = 0
            ltan = 0        'initially assume V[0] = both tangents
            eprev = isLeft(V(0), V(1), P)
            For i = 1 To n - 1
                If i < n - 1 Then
                    enext = isLeft(V(i), V(i + 1), P)
                Else
                    enext = isLeft(V(i), V(0), P)
                End If
                If ((eprev <= 0) And (enext > 0)) Then
                    If (Not (below(P, V(i), V(rtan)))) Then
                        rtan = i
                    End If

                ElseIf ((eprev > 0) And (enext <= 0)) Then
                    If (Not (above(P, V(i), V(ltan)))) Then
                        ltan = i
                    End If
                End If
                eprev = enext
            Next i
        End Sub

        Sub AddSectionToPath(ByVal pth As Drawing2D.GraphicsPath, ByVal pathtype As PathTypeEnum, _
           ByVal pnt1 As PointF, ByVal pnt2 As PointF, ByVal pa As PlaneAngle) _
           Implements IGeometricArea.AddSectionToPath

            Dim i1, i2 As Integer

            i1 = GotoPoint(pnt1)
            i2 = GotoPoint(pnt2)

            If pathtype = PathTypeEnum.PT_BACK_ARC Then
                pth.AddLine(pnt1, pnt2)
            ElseIf pathtype = PathTypeEnum.PT_CHORD Then
                pth.AddLine(pnt1, pnt2)
            ElseIf pathtype = PathTypeEnum.PT_FRONT_ARC Then
                pth.AddLine(pnt1, pnt2)
            Else
                pth.AddLine(pnt1, pnt2)
            End If


        End Sub

        '=========================================================================
        ' fastBall(): a fast approximation of the bounding ball for a point set
        '               based on the algorithm given by [Jack Ritter, 1990]
        '    Input:  an array V[] of n points
        '    Output: a bounding ball = {Point center; float radius;}
        ' http://softsurfer.com/Archive/algorithm_0107/algorithm_0107.htm
        '
        Private Function fastBall(ByVal V() As PointF, ByVal n As Integer) As CircleF
            Dim C As PointF                           ' Center of ball
            Dim rad, rad2 As Single                   ' radius and radius squared
            Dim xmin, xmax, ymin, ymax As Single      ' bounding box extremes
            Dim Pxmin, Pxmax, Pymin, Pymax As Integer    ' index of V() at box extreme
            Dim i As Integer

            ' find a large diameter to start with
            ' first get the bounding box and V() extreme points for it
            xmin = V(0).X
            xmax = V(0).X
            ymin = V(0).Y
            ymax = V(0).Y
            Pymax = 0
            Pymin = 0
            Pxmax = 0
            Pxmin = 0
            For i = 1 To n - 1
                If (V(i).X < xmin) Then
                    xmin = V(i).X
                    Pxmin = i
                ElseIf (V(i).X > xmax) Then
                    xmax = V(i).X
                    Pxmax = i
                End If
                If (V(i).Y < ymin) Then
                    ymin = V(i).Y
                    Pymin = i
                ElseIf (V(i).Y > ymax) Then
                    ymax = V(i).Y
                    Pymax = i
                End If
            Next i

            ' select the largest extent as an initial diameter for the ball
            Dim dVx As Vector2 = New Vector2(V(Pxmax).X - V(Pxmin).X, V(Pxmax).Y - V(Pxmin).Y) ' diff of Vx max and min
            Dim dVy As Vector2 = New Vector2(V(Pymax).X - V(Pymin).X, V(Pymax).Y - V(Pymin).Y) ' diff of Vy max and min
            Dim dx2 As Single = dVx.LengthSquared ' Vx diff squared
            Dim dy2 As Single = dVy.LengthSquared ' Vy diff squared
            If (dx2 >= dy2) Then                     ' x direction is largest extent
                C.X = V(Pxmin).X + Vector2.Multiply(dVx, 0.5).X       ' Center = midpoint of extremes
                C.Y = V(Pxmin).Y + Vector2.Multiply(dVx, 0.5).Y
                rad2 = New Vector2(V(Pxmax).X - C.X, V(Pxmax).Y - C.Y).LengthSquared        ' radius squared
            Else                                 ' y direction is largest extent
                C.X = V(Pymin).X + Vector2.Multiply(dVy, 0.5).X          ' Center = midpoint of extremes
                C.Y = V(Pymin).Y + Vector2.Multiply(dVy, 0.5).Y
                rad2 = New Vector2(V(Pymax).X - C.X, V(Pymax).Y - C.Y).LengthSquared        ' radius squared
            End If
            rad = CSng(Math.Sqrt(rad2))

            ' now check that all points V(i) are in the ball
            ' and if not, expand the ball just enough to include them
            Dim dV As Vector2
            Dim dist, dist2 As Single
            For i = 0 To n - 1
                dV = New Vector2(V(i).X - C.X, V(i).Y - C.Y)
                dist2 = dV.LengthSquared
                If (dist2 > rad2) Then
                    ' V(i) not in ball, so expand ball to include it
                    dist = CSng(Math.Sqrt(dist2))
                    rad = (rad + dist) / 2         ' enlarge radius just enough
                    rad2 = rad * rad
                    dV = Vector2.Multiply(dV, (dist - rad) / dist)
                    C.X = C.X + dV.X ' shift Center toward V(i)
                    C.Y = C.Y + dV.Y
                End If
            Next i
            Dim ret As New CircleF(C, rad)
            Return ret
        End Function

    End Structure


End Module
