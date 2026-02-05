$sourceFolder   = "C:\input"
$exportFolder   = "C:\output\抽出結果"

# 出力フォルダーがなければ作る
if (-not (Test-Path $exportFolder)) { New-Item -Path $exportFolder -ItemType Directory }

Get-ChildItem -Path $sourceFolder -File -Recurse -Filter "*.txt" |
    ForEach-Object {
        $destName = $_.FullName -replace [regex]::Escape($sourceFolder), ""
        $destName = $destName -replace '[\\/]', '_'
        $destPath = Join-Path $exportFolder $destName

        "===== $($_.Name) =====" | Out-File -FilePath $destPath -Encoding utf8
        Get-Content $_.FullName -Encoding UTF8 -Raw | Out-File -FilePath $destPath -Encoding utf8 -Append
    }