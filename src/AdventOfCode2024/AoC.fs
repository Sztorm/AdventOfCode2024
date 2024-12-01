module AoC

open System.IO

let private basePath = Directory.GetParent(Directory.GetCurrentDirectory()).Parent.Parent.FullName;

let readInput (path: string): string array = File.ReadAllLines(Path.Combine(basePath, path))