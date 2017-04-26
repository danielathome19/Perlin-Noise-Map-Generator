(*
 *
 *  Author: Daniel Szelogowski
 *  Created: 4/12/17
 *  Purpose: General purpose Perlin Noise library in F# with map generator
 *
 *)

open System
open System.IO
open System.Drawing
open System.Drawing.Imaging

type Noise = class
    val seed :int         
    val octaves :float    
    val persistence :float
    val smoothing :float
    
    new(Seed :int, Octaves :float, Persistence :float, Smoothing :float) = {seed = Seed; octaves = Octaves; persistence = Persistence; smoothing = Smoothing; }
    new(Seed :int, Octaves :float, Persistence :float) = Noise(Seed, Octaves, Persistence, 4.0)
    new(Seed :int, Octaves :float) = Noise(Seed, Octaves, 0.25)
    new(Seed :int) = Noise(Seed, 4.0, 0.25)

    member this.rawNoise(x :float) =
        let n :int = ((int)x <<< 13) ^^^ ((int)x)
        (1.0f - (float32)((n * (n * n * 15731 * this.seed + 789221 * this.seed) + 1376312589 * this.seed) &&& 0x7fffffff) / (float32)1073741824.0)

    member this.rawNoise2D(x :float, y :float) =
        this.rawNoise(x + y * 57.0)

    member this.smoothNoise(x :float) =
        let left = this.rawNoise(x - 1.0)
        let right = this.rawNoise(x - 1.0)
        (this.rawNoise(x) / 2.0f) + (left / (float32)this.smoothing) + (right / (float32)this.smoothing);

    member this.smoothNoise2D(x :float, y :float) =
        let corners = this.rawNoise2D(x - 1.0, y - 1.0) + this.rawNoise2D(x - 1.0, y + 1.0) + this.rawNoise2D(x + 1.0, y - 1.0) + this.rawNoise2D(x + 1.0, y + 1.0)
        let sides = this.rawNoise2D(x, y - 1.0) + this.rawNoise2D(x, y + 1.0) + this.rawNoise2D(x - 1.0, y) + this.rawNoise2D(x + 1.0, y)
        let center = this.rawNoise2D(x,y)
        (center / 4.0f) + (sides / 8.0f) + (corners / 16.0f)
    
    member this.linearInterpolate(a :float, b :float, x : float) = 
        a * (1.0 - x) + b * x

    member this.cosineInterpolate(a :float, b :float, x :float) = 
        let f = (1.0 - (float)(Math.Cos(x * Math.PI))) / 2.0
        a * (1.0 - f) + b * f

    member this.interpolateNoise(x :float) =
        this.cosineInterpolate((float)(this.smoothNoise((float)(Math.Floor(x)))), (float)(this.smoothNoise((float)(Math.Floor(x)) + 1.0)), (float)(x - (Math.Floor(x))));

    member this.interpolateNoise2D(x :float, y :float) =
        let a = this.cosineInterpolate((float)(this.smoothNoise2D((float)(Math.Floor(x)), (float)(Math.Floor(y)))), (float)(this.smoothNoise2D((float)(Math.Floor(x)) + 1.0, (float)(Math.Floor(y)))), x - (float)(Math.Floor(x)))
        let b = this.cosineInterpolate((float)(this.smoothNoise2D((float)(Math.Floor(x)), (float)(Math.Floor(y)) + 1.0)), (float)(this.smoothNoise2D((float)(Math.Floor(x)) + 1.0, (float)(Math.Floor(y)) + 1.0)), x - (float)(Math.Floor(x)))
        this.cosineInterpolate(a, b, y - (float)(Math.Floor(y)))

    member this.perlinNoise(x :float) =
        let mutable total :float = 0.0
        let mutable frequency :float = 0.0
        let mutable amplitude :float = 0.0

        for i = 0 to (int)this.octaves - 1 do
            frequency <- (float)(Math.Pow(2.0, (float)i))
            amplitude <- (float)(Math.Pow(this.persistence, (float)i))
            total <- total + this.interpolateNoise(x * frequency) * amplitude
        total

    member this.perlinNoise2D(x :float, y :float) = 
        let mutable total :float = 0.0
        let mutable frequency :float = 0.0
        let mutable amplitude :float = 0.0

        for i = 0 to (int)this.octaves - 1 do
            frequency <- (float)(Math.Pow(2.0, (float)i))
            amplitude <- (float)(Math.Pow(this.persistence, (float)i))
            total <- total + this.interpolateNoise2D(x * frequency, y * frequency) * amplitude
        total
    
    member this.perlinNoiseMap(w :int, h :int) = 
        let mutable noise :float[,] = Array2D.zeroCreate w h
        for y = 0 to h - 1 do
            for x = 0 to w - 1 do
                noise.[x, y] <- this.perlinNoise2D((float)x, (float)y)
        noise
end

[<EntryPoint>]
let main argv = 
    printf "Enter image width: "
    let width :int = Convert.ToInt32(Console.ReadLine())
    printf "Enter image height: "
    let height :int = Convert.ToInt32(Console.ReadLine())

    let rand = new Random()
    let n = new Noise(rand.Next(500), (float)16)

    let mapper = n.perlinNoiseMap(width, height)
    let bmp :Bitmap = new Bitmap(width, height)

    Console.WriteLine("Generating bitmap");

    for j = 0 to mapper.GetLength(0) - 1 do
        for k = 0 to mapper.GetLength(1) - 1 do
            let mutable curF :float = mapper.[j, k]

            curF <- Math.Abs(Math.Round(curF, 2))
            curF <- curF * 255.0
            curF <- Math.Round(curF)

            bmp.SetPixel(j, k, Color.FromArgb((int)curF, (int)curF, (int)curF))
            
            printf "%g\t" curF
        printfn ""
    
    printfn "\nSaving bitmap to %A" (Directory.GetCurrentDirectory())

    bmp.Save("PerlinNoiseMap.png", ImageFormat.Png)
    
    printfn "Image successfully saved"

    Console.ReadKey() |> ignore
    0
