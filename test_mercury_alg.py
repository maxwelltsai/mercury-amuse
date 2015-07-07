from amuse.lab import *
from amuse.community.mercury.interface import Mercury
import numpy as np

t=0|units.yr
t_end=100|units.yr

mer = Mercury(redirection='none')
mer.commit_parameters()
print mer.parameters

ss = new_solar_system()
mer.particles.add_particles(ss)
mer.commit_particles()
ek = mer.kinetic_energy + mer.potential_energy
print ek

nbod = 3
# ax = np.array([1.1e-10|(units.AU/units.day**2), 1.2e-10|(units.AU/units.day**2), 1.3e-10|(units.AU/units.day**2)])
# ay = np.array([2.1e-10|(units.AU/units.day**2), 2.2e-10|(units.AU/units.day**2), 2.3e-10|(units.AU/units.day**2)])
# az = np.array([3.1e-10|(units.AU/units.day**2), 3.2e-10|(units.AU/units.day**2), 3.3e-10|(units.AU/units.day**2)])

ax = [1.1e-10, 1.2e-10, 1.3e-10]|(units.AU/units.day**2)
ay = [2.1e-10, 2.2e-10, 2.3e-10]|(units.AU/units.day**2)
az = [3.1e-10, 3.2e-10, 3.3e-10]|(units.AU/units.day**2)

mer.set_user_defined_force(ax, ay, az, nbod)
while t<t_end:
    mer.set_integrator(10)
    # mer.set_user_defined_force(1.1e-10|(units.AU/units.day**2), 1.2e-10|(units.AU/units.day**2), 1.3e-10|(units.AU/units.day**2), 1)
    mer.evolve_model(t)
    print t
    ek = mer.kinetic_energy + mer.potential_energy
    print ek
    t+=5|units.yr
mer.stop()
