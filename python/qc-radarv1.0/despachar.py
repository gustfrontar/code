from multiprocessing import Pool
from slurmpy import Slurm
from datetime import datetime
import pickle
import worker
import sys
from configparser import ConfigParser, ExtendedInterpolation


class Despachar():
    def __init__(self, maxRadar, maxThrporRadar):
        self.maxRadar = maxRadar
        self.maxThreads = maxThrporRadar

    def despachar(self, tareas):
        self._despachar(tareas)


class DespacharLocal(Despachar):
    def __init__(self, maxRadar, maxThrporRadar):
        super().__init__(maxRadar, maxThrporRadar)
        self.pool = Pool(self.maxRadar)

    def _despachar(self, tareas):
        data = self.pool.starmap(worker.worker, tareas)


class DespacharSlurm(Despachar):
    def __init__(self, maxRadar, maxThrporRadar):
        super().__init__(maxRadar, maxThrporRadar)
        config = ConfigParser(interpolation=ExtendedInterpolation())
        config.optionxform = str
        config.read("config.ini")
        SLURMCONF = {"partition": config["SLURM"]["PARTITION"],
                     "N": config["SLURM"]["MAXNODE"],
                     "c":config["SLURM"]["CORES"] ,
                     "n": 1,
		     "d":"singleton",
                     "time":config["SLURM"]["MAXTIME"]}
        jobname = "QC_RADAR_{}".format(round(datetime.timestamp(datetime.now())))
        jobname = "QC_RADAR"
        self.Slurm = Slurm(jobname, SLURMCONF, bash_strict=False)
        self.condaenv = config["System"]["CONDAENV"]

    def _despachar(self, tareas):
        filename = "tareasSlurm.pkl"
        with open(filename, "wb") as fh:
            pickle.dump(tareas, fh)
        data = self.Slurm.run("""
            source activate $condaenv
            python despachar.py $filename $maxRadar $maxThrporRadar
            """,cmd_kwargs={"filename":filename,"maxRadar":self.maxRadar,"maxThrporRadar":self.maxThreads,"condaenv":self.condaenv})


if __name__ == "__main__" :
    filename = sys.argv[1]
    maxRadar = int(sys.argv[2])
    maxThrporRadar = int(sys.argv[3])
    with open(filename, "rb") as fh:
        tareas = pickle.load(fh)
    dispach = DespacharLocal(maxRadar, maxThrporRadar)
    dispach.despachar(tareas)
