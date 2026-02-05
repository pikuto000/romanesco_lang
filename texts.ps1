$sourceFolder   = "C:\Users\Cocoa\romanesco\scala"
$exportFolder   = "C:\Users\Cocoa\romanesco\text"

# 出力フォルダーがなければ作る
if (-not (Test-Path $exportFolder)) { New-Item -Path $exportFolder -ItemType Directory }

Get-ChildItem -Path $sourceFolder -File -Recurse -Filter "*.scala" |
    ForEach-Object {
        $destName = $_.FullName -replace [regex]::Escape($sourceFolder), ""
        $destName = $destName -replace '.scala', '.txt'
        $destPath = Join-Path $exportFolder $destName

        "===== $($_.Name) =====" | Out-File -FilePath $destPath -Encoding utf8
        Get-Content $_.FullName -Encoding UTF8 -Raw | Out-File -FilePath $destPath -Encoding utf8 -Append
    }