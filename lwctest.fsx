#load "lwc.fsx"

open System.Windows.Forms
open System.Drawing

open Lwc

type PINode() as this =
  inherit LWControl()
  let mutable text = "Nodo"

  do
    this.CoordinateType <- World

  member this.Text
    with get() = text
    and set(v) = text <- v
  override this.OnPaint e =
    let parent = this.Parent
    let g = e.Graphics
    
    let sz = this.Size
    let p = this.Position
    g.DrawEllipse(Pens.Red,p.X,p.Y,sz.Width, sz.Height)
    let ssz = g.MeasureString(text, parent.Font)
    
    let sx, sy = p.X + (sz.Width - ssz.Width) / 2.f, p.Y + (sz.Height - ssz.Height) / 2.f
    g.DrawString(text, parent.Font, Brushes.Black, PointF(sx, sy))

type PIEdge() as this =
  inherit LWControl()
  let mutable startN = PINode()
  let mutable endN = PINode()
  let mutable text = "Arco"

  do
    this.CoordinateType <- World


  member this.Text
    with get() = text
    and set(v) = text <- v
  member this.StartNode with get() = startN
                        and set(n) = startN <- n
  member this.EndNode with get() = endN
                        and set(n) = endN <- n
  override this.OnPaint e =
    let parent = this.Parent
    let g = e.Graphics
    let p1 = PointF(startN.Position.X+startN.Size.Width/2.f ,startN.Position.Y+startN.Size.Height/2.f)
    let p2 = PointF(endN.Position.X+endN.Size.Width/2.f ,endN.Position.Y+endN.Size.Height/2.f)
    this.Position <- PointF( (p1.X+p2.X)/2.f, (p1.Y+p2.Y)/2.f)
    
    g.DrawLine(new Pen(Color.Aquamarine, 5.00F), p1,p2)
    g.DrawRectangle(Pens.BurlyWood, new RectangleF(this.Position,this.Size) |> RectF2Rect)
    g.DrawString(text, parent.Font, Brushes.Blue, this.Position)

type Selector =
    | Node of PINode
    | Edge of PIEdge
    | No

type Action =
    | AddNode = 0
    | AddEdge = 1
    | DeleteE = 2

type Box() as this =
  inherit LWContainer()

  let mutable nodes = ResizeArray<PINode>()
  let mutable edge = ResizeArray<PIEdge>()

  let mutable Animations = ResizeArray<PINode*PointF*PointF>()

  let mutable Drag: (PINode * PointF * PointF)option = None //nodo, delta, pos per la distanza nel drag

  let mutable action: Action = Action.AddNode //default

  let mutable Selected: Selector*Selector = No,No

  let mutable animated = false

  let timer = new Timer(Interval=1000/60)     

  do
    timer.Start()//avvio il timer
    timer.Tick.Add(fun _ ->
      for idx in (Animations.Count - 1) .. -1 .. 0 do
        let mutable (n,p,v) = Animations.[idx];
        let mutable V = PointF(0.f,0.f)
        edge |> Seq.iter(fun e ->
          let mutable found = false //se lo trovo
          let mutable End = PINode() //altro arco
          if n = e.StartNode then
            End <- e.EndNode
            found <- true
          if n = e.EndNode then
            End <- e.StartNode
            found <- true
          if found then
            let blengthV = PointF(p.X - End.Position.X, p.Y - End.Position.Y)
            let blength = Norm(blengthV) //distanza tra p e End quindi allungamento della molla a riposo

            let alenghtV = PointF(n.Position.X - End.Position.X, n.Position.Y - End.Position.Y)
            let alenght = Norm(alenghtV) //distanza tra n e End quindi allungamento della molla attuale

            let diff = blength - alenght //allungamento attuale se >0 allungato altrimenti compresso

            let v = MulPointF(SubPointF(End.Position,n.Position),1.f/Norm(SubPointF(End.Position,n.Position))) //vettore direzione tra End e n lungo 1

            V <- SumPointF(V, MulPointF(v,-diff*0.1f)) // V+=-v*diff*0.1
          )
        v <- SumPointF(MulPointF(v,0.9f), V) //aggiorno la vecchia velocita' v = v*0.9+V
        if Norm(v) > 0.1f then
          n.Position <- SumPointF(n.Position,v) //sommo spostamento V alla posizione attuale
          Animations.[idx] <- (n,p,v)
        else
          Animations.RemoveAt(idx)
        this.Invalidate()
      )

  member this.Azione with get() = action
                        and set(a) = action <- a

  member this.Animated with get() = animated
                        and set(a) = animated <- a; if a then timer.Start() else timer.Stop()

  member this.NewNode (pw:PointF) = 
    let N = PINode();
    N.Parent <- this
    N.Size <- SizeF(50.f,50.f)
    N.Position <- PointF(pw.X - N.Size.Width/2.f ,pw.Y - N.Size.Height/2.f)
    nodes.Add(N)
    this.LWControls.Add(N)
    match Selected with
    | (_,b) -> Selected <- (Node N, b)
    this.Invalidate()

  member this.NewEdge _ = 
    match Selected with
    | (Node n1),(Node n2) ->
      if n1 <> n2 then
        let A = PIEdge(CoordinateType = World)
        A.Size <- SizeF(40.f,10.f)
        A.Parent <- this
        A.StartNode <- n1
        A.EndNode <- n2
        if not (edge.Exists(fun e -> (e.StartNode = n1 || e.StartNode = n2) && (e.EndNode = n1 || e.EndNode = n2) )) then
          edge.Add(A)
          this.LWControls.Add(A)
    | _ -> ()
    this.Invalidate()

  member this.DeleteEnt _ = 
    match Selected with
    | (Node n),(_) ->
      edge.RemoveAll(fun e -> if e.StartNode = n || e.EndNode = n then this.LWControls.RemoveAll(fun e1 -> e1 = upcast e) |> ignore; true else false) |> ignore;
      nodes.RemoveAll(fun n1 -> n1 = n) |> ignore;
      this.LWControls.RemoveAll(fun n1 -> n1 = upcast n) |> ignore;
    | (Edge e),(_) ->
      edge.RemoveAll(fun e1 -> e1 = e) |> ignore;
      this.LWControls.RemoveAll(fun e1 -> e1 = upcast e) |> ignore;
    | _ -> ()
    Selected <-No,No
    this.Invalidate()

  member this.Drags (p:PointF) =
    match Selected with
    | (Node n),(_) ->
      let pw = this.TransformPoint this.Transform.W2V n.Position
      let mutable start = n.Position
      Animations.RemoveAll(fun (n1,p1,v) -> if n1=n then start <- p1; true else false) |> ignore
      Drag <- Some(n, PointF(pw.X - p.X, pw.Y-p.Y), start) //calcolo da distanza del punto 
    | _ -> ()    

  member this.Up _ = (
      this.Transform.Translate(0.f, 10.f)
      this.Invalidate())
  member this.Down _ = (
      this.Transform.Translate(10.f, 0.f)
      this.Invalidate()
  )
  member this.Left _ = (
      this.Transform.Translate(0.f, -10.f)
      this.Invalidate()
  )
  member this.Right _ = (
      this.Transform.Translate(-10.f, 0.f)
      this.Invalidate()
  )
  member this.RotateL _ = (
      this.Transform.Rotate(10.f)
      this.Invalidate()
  )
  member this.RotateR _ = (
      this.Transform.Rotate(-10.f)
      this.Invalidate()
  )
  member this.ZoomIn _ = (
      this.Transform.Scale(1.1f, 1.1f)
      this.Invalidate())
  member this.ZoomOut _ = (
      this.Transform.Scale(1.f/1.1f, 1.f/1.1f)
      this.Invalidate()
  )

  override this.OnMouseDown e =
    base.OnMouseDown e
    let p = PointF(single e.X, single e.Y)
    let controlsView = this.LWControls |> Seq.filter (fun c -> c.CoordinateType = View)
    match (controlsView |> Seq.tryFind (fun c -> c.HitTest p)) with
    | Some c -> ()
    | None ->
      let pw = this.TransformPoint this.Transform.V2W p
      let mutable found = false
      match (nodes |> Seq.tryFind(fun c -> c.HitTest pw)) with
      | Some c ->
        found <- true
        match Selected with
        | (a,_) -> Selected <- (Node c, a)
        match action with
        | Action.DeleteE ->
          this.DeleteEnt()
        | Action.AddEdge ->
          match Selected with
          | (Node a, Node b) ->
            this.NewEdge()
          | _ -> ()
        | _ -> this.Drags(PointF(float32 e.X, float32 e.Y))
      | None -> ()

      if not found then
        match (edge |> Seq.tryFind(fun c -> c.HitTest pw)) with
        | Some c ->
          found <- true
          match Selected with
          | (a,_) -> Selected <- (Edge c, a)
          match action with 
          | Action.DeleteE ->
            this.DeleteEnt()
          | _ -> ()
        | None -> ()
        
      if not found then
        match Selected with
        | (a,_) -> Selected <- (No, a)
        match action with
        | Action.AddNode ->
          this.NewNode(pw)
        | _ -> ()

  override this.OnMouseUp e =
    base.OnMouseUp e
    match Drag with
    | Some(n,d,p) ->
      if animated then
        if edge.Exists(fun e -> e.EndNode = n || e.StartNode = n) && not(Animations.Exists(fun (n1,p1,v1) -> n1 = n)) then
          Animations.Add((n,p,PointF())) //p è lo stato originale che mi serve per calcolare la posizione iniziale da dove inizio a tirare
    | _ -> ()
    Drag <- None

  override this.OnMouseMove e =
    base.OnMouseMove e
    match Drag with
    | Some (n, d, p) ->
      let pa = PointF(float32 e.X + d.X, float32 e.Y + d.Y)
      let pw = this.TransformPoint this.Transform.V2W pa
      n.Position <- pw
      this.Invalidate()
    | None -> ()

  override this.OnKeyDown e =
    base.OnKeyDown e


type PIButton() =
  inherit LWControl()
  let mutable text = ""
  member this.Text
    with get() = text
    and set(v) = text <- v

  override this.OnPaint e =
    let parent = this.Parent
    let g = e.Graphics

    let r = RectangleF(this.Position, this.Size) |> RectF2Rect
    g.FillRectangle(Brushes.White,r)
    g.DrawRectangle(Pens.Black, r)

    let ssz = g.MeasureString(text, parent.Font)
    let p = this.Position
    let sz = this.Size
    let sx, sy = p.X + (sz.Width - ssz.Width) / 2.f, p.Y + (sz.Height - ssz.Height) / 2.f
    g.DrawString(text, parent.Font, Brushes.Black, PointF(sx, sy))

let node = PIButton(Position = PointF(475.f,25.f), Size=SizeF(30.f,30.f), Text="Nodo")
let arco = PIButton(Position = PointF(475.f,75.f), Size=SizeF(30.f,30.f), Text="Arco")
let remove = PIButton(Position = PointF(475.f,125.f), Size=SizeF(40.f,30.f), Text="Elimina")
let zoom = PIButton(Position=PointF(475.f, 175.f),Size=SizeF(30.f, 30.f),Text="+")
let dezoom = PIButton(Position=PointF(475.f, 225.f),Size=SizeF(30.f, 30.f),Text="-")
let ruotaR = PIButton(Position=PointF(475.f, 275.f),Size=SizeF(40.f, 30.f),Text="RuotaR")
let ruotaL = PIButton(Position=PointF(475.f, 325.f),Size=SizeF(40.f, 30.f),Text="RuotaL")
let anima = PIButton(Position=PointF(475.f, 375.f),Size=SizeF(40.f, 30.f),Text="Anima")
let su = PIButton(Position=PointF(475.f, 425.f),Size=SizeF(30.f, 30.f),Text="W")
let giu = PIButton(Position=PointF(445.f, 455.f),Size=SizeF(30.f, 30.f),Text="S")
let dx = PIButton(Position=PointF(505.f, 455.f),Size=SizeF(30.f, 30.f),Text="D")
let sx = PIButton(Position=PointF(475.f, 455.f),Size=SizeF(30.f, 30.f),Text="A")

let c = new Box(Dock=DockStyle.Fill)
node.Parent <- c
arco.Parent <- c
remove.Parent <- c
zoom.Parent <- c
dezoom.Parent <- c
ruotaR.Parent <- c
ruotaL.Parent <- c
anima.Parent <- c
su.Parent <- c
giu.Parent <- c
dx.Parent <- c
sx.Parent <- c

node.MouseDown.Add(fun _ ->
    c.Azione <- Action.AddNode)
arco.MouseDown.Add(fun _ ->
    c.Azione <- Action.AddEdge)
remove.MouseDown.Add(fun _ ->
    c.Azione <- Action.DeleteE)
zoom.MouseDown.Add(fun _ ->
    c.ZoomIn())
dezoom.MouseDown.Add(fun _ ->
    c.ZoomOut())
ruotaR.MouseDown.Add(fun _ ->
    c.RotateR())
ruotaL.MouseDown.Add(fun _ ->
    c.RotateL())
anima.MouseDown.Add(fun _ ->
    c.Animated <- not(c.Animated)
    if c.Animated then
        anima.Text <- "Stop"
    else
        anima.Text <- "Anima"
    c.Invalidate())
su.MouseDown.Add(fun _ ->
    c.Up())
giu.MouseDown.Add(fun _ ->
    c.Down())
dx.MouseDown.Add(fun _ ->
    c.Right())
sx.MouseDown.Add(fun _ ->
    c.Left())



c.LWControls.Add(node)
c.LWControls.Add(arco)
c.LWControls.Add(remove)
c.LWControls.Add(zoom)
c.LWControls.Add(dezoom)
c.LWControls.Add(ruotaR)
c.LWControls.Add(ruotaL)
c.LWControls.Add(anima)
c.LWControls.Add(su)
c.LWControls.Add(giu)
c.LWControls.Add(dx)
c.LWControls.Add(sx)
let f = new Form()
f.Size <- Size(700,700)
f.Controls.Add(c)
f.Show()
