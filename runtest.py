import promptlib
import subprocess
import os.path
from shutil import copyfile

def getDir():
    cachePath = ".cache"
    if os.path.isfile(cachePath):
        with open(cachePath, "r") as cacheFile:
            wd = cacheFile.read()
            if os.path.isdir(os.path.join(wd, "src")):
                return wd
    while(True):
        prompter = promptlib.Files()
        dir = prompter.dir()
        if os.path.isdir(os.path.join(dir, "src")):
            with open(cachePath, "w") as cacheFile:
                cacheFile.write(dir)
            return dir


if __name__ == "__main__":
    print("Fetching latest version of the tester...")
    pull_output = subprocess.check_output(["git", "pull"])
    version = subprocess.check_output(["git", "rev-parse", "HEAD"])
    print(f"Running with version {version.decode('ascii')}")

    dir = getDir()
    isCw2 = dir.endswith("-spark")
    suffix = "cw2" if isCw2 else "cw1"
    copyfile(
        os.path.join(os.getcwd(), f"ImdbSuiteTester.{suffix}.scala"),
        os.path.join(dir, "src", "test", "scala", "imdb", "ImdbSuiteTester.scala"))
    res = subprocess.run("sbt test", shell=True, cwd=dir)
    os.remove(os.path.join(dir, "src", "test", "scala", "imdb", "ImdbSuiteTester.scala"))
