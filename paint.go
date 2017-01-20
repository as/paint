package main

import (
	//	"github.com/as/clip"
	"fmt"
	"golang.org/x/exp/shiny/driver"
	"golang.org/x/exp/shiny/screen"
	"golang.org/x/mobile/event/key"
	"golang.org/x/mobile/event/lifecycle"
	"golang.org/x/mobile/event/mouse"
	"golang.org/x/mobile/event/paint"
	"golang.org/x/mobile/event/size"
	"image"
	"image/color"
	"image/color/palette"
	_ "image/png"
	"io"
	"log"
	"math"
	"net"
	"os"
	"time"
	//"encoding/binary"
	"bytes"
	"flag"
	//"github.com/as/frame"
	"golang.org/x/image/draw"
	"golang.org/x/image/math/f64"
)

var (
	cos30 = math.Cos(math.Pi / 6)
	sin30 = math.Sin(math.Pi / 6)
)

// {{A,B,A*x-x+B*y},
// {-B, A, -B*x+A*y-y},

func RotateMask(r image.Rectangle) image.Image {
	var (
		dst *image.RGBA
		rr image.Rectangle
	)
	func(){
		if dst != nil && r == rr {
			return
		}
		dst = image.NewRGBA(r)
		col := image.NewUniform(color.RGBA{255,255,255,25})
		for i := 1; i < 10; i++{
		Ellipse(
			dst,
			image.Pt((r.Max.X-r.Min.X)/2, (r.Max.Y-r.Min.Y)/2),
			col,
			r.Dx()/i,
			r.Dy()/i,
			int(1), image.ZP, int(1), int(1),
		)
		}
		rr = r
	}()
	return dst
}

var Sin5, Cos5 = math.Sin(5), math.Cos(5)
var Sin1, Cos1 = math.Sin(1), math.Cos(1)

func Rotate5(about image.Point) f64.Aff3 {
	A := Cos1
	B := Sin1
	x := float64(about.X)
	y := float64(about.Y)
	return f64.Aff3{
		A, B, A*x - x + B*y,
		-B, A, -B*x + A*y - y,
	}
}

func Rotate(t float64, about image.Point) f64.Aff3 {
	A := math.Cos(t)
	B := math.Sin(t)
	x := float64(about.X)
	y := float64(about.Y)
	return f64.Aff3{
		A, B, A*x - x + B*y,
		-B, A, -B*x + A*y - y,
	}
}

var remote = flag.String("s", ":443", "endpoint to connect to or listen on (if host is omitted)")

func init() {
	flag.Parse()
}

func decode(filename string) (draw.Image, error) {
	f, err := os.Open(filename)
	if err != nil {
		return nil, err
	}
	defer f.Close()
	m, _, err := image.Decode(f)
	if err != nil {
		return nil, fmt.Errorf("could not decode %s: %v", filename, err)
	}
	if t, ok := m.(draw.Image); ok {
		return t, nil
	}
	panic("not a draw.Image")
}
func mustdecode(file string) draw.Image {
	img, err := decode(file)
	if err != nil {
		log.Fatalln(err)
	}
	return img
}

var winSize = image.Pt(2560, 1440)

var cnt int

//wire9 Point X[4,int32] Y[4,int32]
//wire9 Rectangle Min[,Point] Max[,Point]
//wire9 Drawd hdr[1] dstid[4] srcid[4] maskid[4] r[,Rectangle] sp[,Point] maskp[,Point]
//wire9 DrawE hdr[1] dstid[4] srcid[4] c[8,Point] a[4] b[4] thick[4] sp[8,Point] alpha[4] phi[4]
//wire9 DrawAF hdr[1] dstid[4] srcid[4] r[,Rectangle] sp[,Point]

func (r Rectangle) Canon() image.Rectangle {
	return image.Rect(int(r.Min.X), int(r.Min.Y), int(r.Max.X), int(r.Max.Y))
}
func (p Point) Canon() image.Point {
	return image.Pt(int(p.X), int(p.Y))
}

type Drawd struct {
	hdr    byte
	dstid  uint32
	srcid  uint32
	maskid uint32
	r      Rectangle
	sp     Point
	maskp  Point
}

type Client struct {
	id     int
	joined time.Time
	out    chan Msg
}

type Wire interface{
	WriteBinary(w io.Writer) error
}

type Msg struct {
	Client *Client
	Value  interface{}
}

var client bool

var cyan = image.NewUniform(color.RGBA{0, 255, 255, 128})

func main() {
	joinc := make(chan *Client, 1)
	drawin := make(chan Msg)
	selecting := mouse.ButtonNone
	focused := false

	tick := time.NewTicker(time.Millisecond * 25)
	driver.Main(func(src screen.Screen) {
		win, _ := src.NewWindow(&screen.NewWindowOptions{winSize.X, winSize.Y})
		tx, _ := src.NewTexture(winSize)
		buf, _ := src.NewBuffer(winSize)
		tx = tx
		apos := image.ZP
		win.Upload(image.ZP, buf, buf.Bounds())

		// Draw device - anything sent into its out channel gets drawn on the local
		// screen
		devdraw := &Client{id: -1, joined: time.Now(), out: make(chan Msg)}
		go func() {
			src, dst := cyan, buf.RGBA()
			drawGradient(buf.RGBA(), image.Rect(0, 0, 180, 256*7))
			drawGradient(buf.RGBA(), image.Rect(0, 0, 180, 256*7))
			drawGradient(buf.RGBA(), image.Rect(0, 0, 180, 256*7))
			drawGrayscale(buf.RGBA(), image.Rect(256, winSize.Y-256, 512, winSize.Y))
		drawGrayscale(buf.RGBA(), image.Rect(256, winSize.Y-256, 512, winSize.Y))
			for {
				ref := func() {
					select {
					case <-tick.C:
						win.SendFirst(paint.Event{})
					default:
					}
				}
				for e := range devdraw.out {
					switch e := e.Value.(type) {
					case *DrawE:
						go Ellipse(dst, e.c.Canon(), cyan, int(e.a), int(e.b), int(e.thick), image.ZP, int(e.alpha), int(e.phi))
						ref()
					case Drawd:
						r := e.r
						sp := e.sp
						//fmt.Printf("dst=%d r=%v, src=%d, sp=%v\n", e.dstid, r, e.srcid, sp)
						//x := r.Canon()
						//fmt.Println(x)
						draw.Draw(dst, r.Canon(), src, sp.Canon(), draw.Over)
						ref()
					case *DrawAF:
						r := e.r
						sp := e.sp
						//						x := r.Canon()
						//fmt.Println(x)
						go draw.CatmullRom.Transform(dst, Rotate5(sp.Canon()), dst, r.Canon(), draw.Over, 	&draw.Options{
							DstMaskP: sp.Canon(),
							DstMask: RotateMask(r.Canon().Add(sp.Canon())),
							})

						ref()
					case interface{}:
						fmt.Printf("unknown message: %#v\n", e)
					}
				}
			}
		}()
		joinc <- devdraw		

		// dst Image, s2d f64.Aff3, src image.Image, sr image.Rectangle, op Op, opts *Options

		// Forwarder
		// - registers remote clients
		// - sends draw messages generated by GUI actions to the local and remote clients
		go func() {
			clients := make(map[int]*Client)
			n := 1
			for {
				select {
				case c := <-joinc:
					if c.id == 0 {
						c.id = n
						n++
					}
					fmt.Printf("Client %d joined\n", c.id)
					clients[c.id] = c
				case msg := <-drawin:
					for _, c := range clients {
						//fmt.Println("send it to client", c.id)
						if msg.Client.id == c.id {
							//fmt.Println("skip client", c.id)
							continue
						}
						if client && msg.Client.id != -1 && c.id != -1 {
							//fmt.Println("client skip", c.id)
							continue
						}
						c.out <- msg
					}
				}
			}
		}()

		handle := func(conn net.Conn, c *Client) {
			go func(w io.Writer) {
				for msg := range c.out {
					d := msg.Value.(Wire)
					bw := new(bytes.Buffer)
					d.WriteBinary(bw)
					//fmt.Printf("drawout: send: %q\n", bw.Bytes())
					bw.WriteTo(w)
				}
			}(conn)

			buf := make([]byte, 8192)
			for {
				n, err := conn.Read(buf)
				if err != nil {
					return
				}
				var m interface{}
				switch buf[0]{
				case 'E': 
					e := &DrawE{}
					e.ReadBinary(bytes.NewReader(buf[:n]))
					m = e
				case 'A':
					a := &DrawAF{}
					a.ReadBinary(bytes.NewReader(buf[:n]))
					m = a
				default:
					fmt.Println("unknown draw message")
					continue
				}
				drawin <- Msg{Client: c, Value: m}
			}
		}

		// Listener - accept new connections and send them to the forwarder
		go func() {
			host, port, err := net.SplitHostPort(*remote)
			ck(err)
			if host == "" {
				fmt.Println("host is", host)
				fd, err := net.Listen("tcp", ":"+port)
				ck(err)
				for {
					conn, err := fd.Accept()
					if err != nil {
						continue
					}
					c := &Client{
						joined: time.Now(),
						out:    make(chan Msg),
					}
					joinc <- c
					go handle(conn, c)
				}
			}

			fmt.Println("connect as client")
			conn, err := net.Dial("tcp", *remote)
			ck(err)
			c := &Client{
				joined: time.Now(),
				out:    make(chan Msg),
			}
			client = true
			joinc <- c
			handle(conn, c)
		}()

		// UI - event loop
		brush := &Brush{
			image.Rect(-2, -2, 2, 2),
			1,
			1,
		}
		for {
			switch e := win.NextEvent().(type) {
			case key.Event:
				if e.Direction != key.DirPress && e.Direction != key.DirNone {
					break
				}
				r := brush.Rectangle
				switch e.Code {
				case key.CodeUpArrow:
					brush.Rectangle = image.Rect(
						r.Min.X-brush.dx, r.Min.Y-brush.dy,
						r.Max.X+brush.dx, r.Max.Y+brush.dy,
					)
				case key.CodeDownArrow:
					brush.Rectangle = image.Rect(
						r.Min.X+brush.dx, r.Min.Y+brush.dy,
						r.Max.X-brush.dx, r.Max.Y-brush.dy,
					)
				case key.CodeLeftArrow:
				case key.CodeDeleteBackspace:
				case key.CodeReturnEnter:
				default:
				}
				win.Send(paint.Event{})
			case mouse.Event:
				apos = image.Pt(int(e.X), int(e.Y))
				if e.Button == mouse.ButtonRight {
					if e.Direction == mouse.DirPress {
						cyan = image.NewUniform(buf.RGBA().At(apos.X, apos.Y))
					}
				}
				if selecting != mouse.ButtonNone {
					cnt++
					r := brush.Add(apos)
					if selecting == mouse.ButtonMiddle {
						AF := &DrawAF{
							hdr:   'A',
							dstid: 0,
							srcid: 0,
							r: Rectangle{
								Point{int32(r.Min.X), int32(r.Min.Y)},
								Point{int32(r.Max.X), int32(r.Max.Y)},
							},
							//sp:    Point{0, 0},
							sp: Point{int32(-apos.X), int32(-apos.Y)},
						}
						bb := new(bytes.Buffer)
						err := (AF).WriteBinary(bb)
						//fmt.Printf("mouse event send %q", bb)
						ck(err)
						devdraw.out <- Msg{devdraw, AF}
						drawin <- Msg{devdraw, AF}
					} else {
						E := &DrawE{
							hdr:   'E',
							dstid: 0,
							srcid: 1,
							c:     Point{int32(apos.X), int32(apos.Y)},
							a:     uint32(brush.Dx()),
							b:     uint32(brush.Dy()),
							thick: uint32(brush.dx),
							sp:    Point{0, 0},
							alpha: 0,
							phi:   1,
						}
						bb := new(bytes.Buffer)
						err := (E).WriteBinary(bb)
						//fmt.Printf("mouse event send %q", bb)
						ck(err)
						devdraw.out <- Msg{devdraw, E}
						drawin <- Msg{devdraw, E}
					}
				}

				if e.Button == mouse.ButtonLeft || e.Button == mouse.ButtonMiddle {
					if e.Direction == mouse.DirPress {
						selecting = e.Button
					}
					if e.Direction == mouse.DirRelease {
						selecting = mouse.ButtonNone
					}
				}
			case size.Event:
			case paint.Event:
				win.Upload(image.ZP, buf, buf.Bounds())
				//win.Copy(buf.Bounds().Min, tx, tx.Bounds(), screen.Over, nil)
				if !focused {
					//win.Copy(buf.Bounds().Min, tx, tx.Bounds(), screen.Over, nil)
				}
				win.Publish()
			case lifecycle.Event:
				if e.To == lifecycle.StageDead {
					return
				}

				// NT doesn't repaint the window if another window covers it
				if e.Crosses(lifecycle.StageFocused) == lifecycle.CrossOff {
					focused = false
				} else if e.Crosses(lifecycle.StageFocused) == lifecycle.CrossOn {
					focused = true
				}
			case interface{}:
				fmt.Printf("Unhandled event: %#v\n", e)
			}
		}
	})
}

type Brush struct {
	image.Rectangle
	dx, dy int
}

func ck(err error) {
	if err != nil {
		log.Fatalln(err)
	}
}

func abs(a int) int {
	if a < 0 {
		return -a
	}
	return a
}

func XEllipse(dst draw.Image, c image.Point, src image.Image, a, b, thick float64, sp image.Point, alpha, phi int) {
	for theta := float64(0); theta <= 2*math.Pi; theta += 0.1 {
		x, y := int(math.Cos(theta)*a), int(math.Sin(theta)*b)
		r := image.Rect(0, 0, 1, 1).Add(c)
		draw.Draw(dst, r.Add(image.Pt(x, y)), src, sp, draw.Src)
	}
}

func Line(dst draw.Image, p0, p1 image.Point, thick int, src image.Image, sp image.Point) {
	//r := image.Rect(0, 0, thick, thick)

}

var Plan9 = color.Palette(palette.Plan9)

func drawGrayscale(dst draw.Image, r image.Rectangle) {
	c := color.NRGBA{0, 0, 0, 0}
	for x := 0; x < 256; x++ {
		c.R++
		c.G++
		c.B++
		for y := 0; y < 256; y++ {
			c.A++
			dst.Set(x+r.Min.X, y+r.Min.Y, c)
		}

	}
}

func drawGradient(dst draw.Image, r image.Rectangle) {
	lim := r.Dx() * r.Dy()
	x, y := 0, 0
	c := color.RGBA{255, 0, 0, 254}
	step := 255 / float64(r.Dx())
	sum := float64(0)
	for i := 0; i < lim; i++ {
		switch {
		case c.R == 255 && c.G == 0 && c.B == 0:
			c.G++
			y++
			x = 0
			sum = 0
		case c.R == 255 && c.G != 255 && c.B == 0:
			c.G++
		case c.G == 255 && c.R != 0:
			c.R--
		case c.R == 0 && c.B != 255:
			c.B++
		case c.B == 255 && c.G != 0:
			c.G--
		case c.G == 0 && c.R != 255:
			c.R++
		default:
			c.B--
		}
		dst.Set(y, x, c)
		x++
	}
	sum = 0
	step = 255 / float64(r.Dx()/2)
	for x := 0; x < r.Dx()/2; x++ {
		draw.Draw(dst, image.Rect(x, 0, x+1, r.Dy()), image.NewUniform(color.NRGBA{255, 255, 255, 255 - byte(sum)}), image.ZP, draw.Over)
		sum += step
	}
	sum = 0
	for x := 128; x < 255; x++ {
		draw.Draw(dst, image.Rect(x, 0, x+1, r.Dy()), image.NewUniform(color.NRGBA{0, 0, 0, byte(sum)}), image.ZP, draw.Over)
		sum += step
	}
}

func drawBorder(dst draw.Image, r image.Rectangle, src image.Image, sp image.Point, thick int) {
	draw.Draw(dst, image.Rect(r.Min.X, r.Min.Y, r.Max.X, r.Min.Y+thick), src, sp, draw.Src)
	draw.Draw(dst, image.Rect(r.Min.X, r.Max.Y-thick, r.Max.X, r.Max.Y), src, sp, draw.Src)
	draw.Draw(dst, image.Rect(r.Min.X, r.Min.Y, r.Min.X+thick, r.Max.Y), src, sp, draw.Src)
	draw.Draw(dst, image.Rect(r.Max.X-thick, r.Min.Y, r.Max.X, r.Max.Y), src, sp, draw.Src)
}

// Ellipse draws a filled ellipse at center point c
// and eccentricity a and b. The thick argument is ignored
// (until a line drawing function is available)
//
// The method uses an efficient integer-based rasterization
// technique originally described in:
//
// McIlroy, M.D.: There is no royal road to programs: a trilogy
// on raster ellipses and programming methodology,
// Computer Science TR155, AT&T Bell Laboratories, 1990
//

func Ellipse(dst draw.Image, c image.Point, src image.Image, a, b, thick int, sp image.Point, alpha, phi int) {
	xc, yc := c.X, c.Y
	var (
		x, y       = 0, b
		a2, b2     = a * a, b * b
		crit1      = -(a2/4 + a%2 + b2)
		crit2      = -(b2/4 + b%2 + a2)
		crit3      = -(b2/4 + b%2)
		t          = -a2 * y
		dxt, dyt   = 2 * b2 * x, -2 * a2 * y
		d2xt, d2yt = 2 * b2, 2 * a2
		incx       = func() { x++; dxt += d2xt; t += dxt }
		incy       = func() { y--; dyt += d2yt; t += dyt }
	)
	point := func(x, y int) {
		draw.Draw(dst, image.Rect(x, y, x+1, yc), src, sp, draw.Over)
		//draw.Draw(dst, image.Rect(x, y, x+1, y-1), src, sp, draw.Over)
		// Perspective-retaining lines
		//draw.Draw(dst, image.Rect(x, y, x+1, yc/2), src, sp, draw.Over)
	}

	for y >= 0 && x <= a {
		point(xc+x, yc+y)
		if x != 0 || y != 0 {
			point(xc-x, yc-y)
		}
		if x != 0 && y != 0 {
			point(xc+x, yc-y)
			point(xc-x, yc+y)
		}
		if t+b2*x <= crit1 || t+a2*y <= crit3 {
			incx()
		} else if t-a2*y > crit2 {
			incy()
		} else {
			incx()
			incy()
		}
	}
}
