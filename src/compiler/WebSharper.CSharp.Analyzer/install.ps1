﻿param($installPath, $toolsPath, $package, $project)

$project.Object.AnalyzerReferences.Add((Join-Path $toolsPath "FSharp.Core.dll"))
$project.Object.AnalyzerReferences.Add((Join-Path $toolsPath "Mono.Cecil.dll"))
$project.Object.AnalyzerReferences.Add((Join-Path $toolsPath "Mono.Cecil.Mdb.dll"))
$project.Object.AnalyzerReferences.Add((Join-Path $toolsPath "Mono.Cecil.Pdb.dll"))
$project.Object.AnalyzerReferences.Add((Join-Path $toolsPath "WebSharper.Core.JavaScript.dll"))
$project.Object.AnalyzerReferences.Add((Join-Path $toolsPath "WebSharper.Core.dll"))
$project.Object.AnalyzerReferences.Add((Join-Path $toolsPath "WebSharper.InterfaceGenerator.dll"))
$project.Object.AnalyzerReferences.Add((Join-Path $toolsPath "WebSharper.Compiler.dll"))
$project.Object.AnalyzerReferences.Add((Join-Path $toolsPath "WebSharper.Compiler.CSharp.dll"))
$project.Object.AnalyzerReferences.Add((Join-Path $toolsPath "WebSharper.CSharp.Analyzer.dll"))
