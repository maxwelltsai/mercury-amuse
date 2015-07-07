module AmuseInterface
contains

function initialize_code() result(ret)
  use amuse_mercuryMod
  use StoppingConditions
  integer :: ret
  integer :: error
  error = set_support_for_condition(TIMEOUT_DETECTION)
  ret=mercury_init()
end function  

function cleanup_code() result(ret)
  use amuse_mercuryMod
  integer :: ret
  ret=mercury_end()
end function  

function commit_particles() result(ret)
  use amuse_mercuryMod
  integer :: ret
  
  ret=finish_init()
end function  

function commit_parameters() result(ret)
  use amuse_mercuryMod
  integer :: ret
  ret=mercury_commit_parameters()
end function  

function recommit_parameters() result(ret)
  use amuse_mercuryMod
  integer :: ret
  ret=0
end function  

function recommit_particles() result(ret)
  use amuse_mercuryMod
  integer :: ret

  ret=finish_init()
end function  

function get_time(time_out) result(ret)
  use amuse_mercuryMod
  integer :: ret
  real*8 :: time_out
  ret=mercury_time(time_out)
end function  

function set_initial_timestep(time_) result(ret)
  use amuse_mercuryMod
  integer :: ret
  real*8 :: time_
  ret=set_initial_timestep_src(time_)
end function  

function get_initial_timestep(time_) result(ret)
  use amuse_mercuryMod
  integer :: ret
  real*8 :: time_
  ret=get_initial_timestep_src(time_)
end function  

function evolve_model(tend) result(ret)
  use amuse_mercuryMod
  integer :: ret
  real*8 :: tend
  print *, 'evolve_model mercury called'
  ret=evolve_mercury(tend)
end function  

function synchronize_model() result(ret)
  integer :: ret
  ret = 0
end function

function new_orbiter(id,mass,dens,x,y,z,vx,vy,vz,sx,sy,sz,celimit) result(ret)
  use amuse_mercuryMod
  integer :: ret,id
  real*8 :: mass,dens,x,y,z,vx,vy,vz,sx,sy,sz,celimit
  ret=add_particle(id,mass,dens,x,y,z,vx,vy,vz,sx,sy,sz,celimit)
end function  

function new_central_particle(id,mass,radius,j2,j4,j6,Lx,Ly,Lz) result(ret)
  use amuse_mercuryMod
  integer :: ret,id
  real*8 :: mass, radius, oblateness(3), spin(3)
  real*8 :: j2, j4, j6, Lx, Lz, Ly
  oblateness(1)=j2;oblateness(2)=j4;oblateness(3)=j6
  spin(1)=Lx;spin(2)=Ly;spin(3)=Lz
  ret=set_central_body(mass=mass, radius=radius, oblateness=oblateness,spin=spin)
end function  

function get_orbiter_state(id,mass,dens,x,y,z,vx,vy,vz,sx,sy,sz,celimit) result(ret)
  use amuse_mercuryMod
  integer :: ret,id
  real*8 :: mass,dens,x,y,z,vx,vy,vz,sx,sy,sz,celimit
  ret=get_particle_state(id,mass,dens,x,y,z,vx,vy,vz,sx,sy,sz,celimit)
  if(id.EQ.1) ret=1
end function

function set_orbiter_state(id,mass,dens,x,y,z,vx,vy,vz,sx,sy,sz,celimit) result(ret)
  use amuse_mercuryMod
  integer :: ret,id
  real*8 :: mass,dens,x,y,z,vx,vy,vz,sx,sy,sz,celimit
  ret=set_particle_state(id,mass,dens,x,y,z,vx,vy,vz,sx,sy,sz,celimit)
  write(6,*) "in set orbiter state", id
  if(id.EQ.1) ret=1
end function

function set_central_particle_state(id,mass,radius,j2,j4,j6,Lx,Ly,Lz) result(ret)
  use amuse_mercuryMod
  integer :: ret,id
  real*8 :: mass, radius, oblateness(3), spin(3),j2,j4,j6,Lx,Ly,Lz
  oblateness(1)=j2;oblateness(2)=j4;oblateness(3)=j6
  spin(1)=Lx;spin(2)=Ly;spin(3)=Lz
  ret=set_central_body(mass=mass, radius=radius, oblateness=oblateness,spin=spin)
end function

function get_central_particle_state(id,mass,radius,j2,j4,j6,Lx,Ly,Lz) result(ret)
  use amuse_mercuryMod
  integer :: ret,id
  real*8 :: mass, radius, oblateness(3), spin(3),j2,j4,j6,Lx,Ly,Lz
  ret=get_central_body(mass=mass, radius=radius, oblateness=oblateness,spin=spin)
  j2=oblateness(1);j4=oblateness(2);j6=oblateness(3)
  Lx=spin(1);Ly=spin(2);Lz=spin(3)
end function

function get_position(id, x, y, z) result(ret)
  use amuse_mercuryMod
  integer :: ret,id
  real*8 :: x,y,z
  ret=get_particle_state(id, x=x, y=y, z=z)
end function

function set_position(id, x, y, z) result(ret)
  use amuse_mercuryMod
  integer :: ret,id
  real*8 :: x,y,z
  ret=set_particle_state(id,x=x,y=y,z=z)
end function

function get_velocity(id, vx, vy, vz) result(ret)
  use amuse_mercuryMod
  integer :: ret,id
  real*8 :: vx,vy,vz
  ret=get_particle_state(id, vx=vx, vy=vy, vz=vz)
end function

function set_velocity(id, vx, vy, vz) result(ret)
  use amuse_mercuryMod
  integer :: ret,id
  real*8 :: vx,vy,vz
  ret=set_particle_state(id, vx=vx, vy=vy, vz=vz)
end function

function delete_particle(id) result(ret)
  use amuse_mercuryMod
  integer :: ret,id
  ret=remove_particle(id)
end function  

function set_density(id, density) result(ret)
  use amuse_mercuryMod
  integer :: ret, id
  real*8 :: density
  ret=set_particle_state(id, dens=density)
end function

function get_density(id, density) result(ret)
  use amuse_mercuryMod
  integer :: ret, id
  real*8 :: density
  ret=get_particle_state(id, dens=density)
end function

function set_mass(id, mass) result(ret)
  use amuse_mercuryMod
  integer :: ret, id
  real*8 :: mass
  ret=set_particle_state(id, mass=mass)
end function

function get_mass(id, mass) result(ret)
  use amuse_mercuryMod
  integer :: ret, id
  real*8 :: mass
  ret=get_particle_state(id, mass=mass)
end function

function set_central_mass(id, mass) result(ret)
  use amuse_mercuryMod
  integer :: ret, id
  real*8 :: mass
  ret=set_central_body(mass=mass)
end function

function get_central_mass(id, mass) result(ret)
  use amuse_mercuryMod
  integer :: ret, id
  real*8 :: mass
  ret=get_central_body(mass=mass)
end function

function set_central_radius(id, radius) result(ret)
  use amuse_mercuryMod
  integer :: ret, id
  real*8 :: radius
  ret=set_central_body(radius=radius)
end function

function get_central_radius(id, radius) result(ret)
  use amuse_mercuryMod
  integer :: ret, id
  real*8 :: radius
  ret=get_central_body(radius=radius)
end function

function set_celimit(id, celimit) result(ret)
  use amuse_mercuryMod
  integer :: ret, id
  real*8 :: celimit
  ret=set_particle_state(id, celimit=celimit)
end function

function get_celimit(id, celimit) result(ret)
  use amuse_mercuryMod
  integer :: ret, id
  real*8 :: celimit
  ret=get_particle_state(id, celimit=celimit)
end function

function set_central_oblateness(id, j2,j4,j6) result(ret)
  use amuse_mercuryMod
  integer :: ret, id
  real*8 :: j2,j4,j6,oblateness(3)
  oblateness(1)=j2;oblateness(2)=j4;oblateness(3)=j6
  ret=set_central_body(oblateness=oblateness)
end function

function get_central_oblateness(id, j2,j4,j6) result(ret)
  use amuse_mercuryMod
  integer :: ret,id
  real*8 :: j2,j4,j6,oblateness(3)
  ret=get_central_body(oblateness=oblateness)
  j2=oblateness(1);j4=oblateness(2);j6=oblateness(3)
end function

function set_central_spin(id, lx,ly,lz) result(ret)
  use amuse_mercuryMod
  integer :: ret, id
  real*8 :: lx,ly,lz,spin(3)
  spin(1)=lx;spin(2)=ly;spin(3)=lz
  ret=set_central_body(spin=spin)
end function

function get_central_spin(id, lx,ly,lz) result(ret)
  use amuse_mercuryMod
  integer :: ret, id
  real*8 :: lx,ly,lz,spin(3)
  ret=get_central_body(spin=spin)
  lx=spin(1);ly=spin(2);lz=spin(3)
end function

function set_angularmomentum(id, Lx,Ly,Lz) result(ret)
  use amuse_mercuryMod
  integer :: ret, id
  real*8 :: Lx,Ly,Lz
  ret=set_particle_state(id, sx=Lx, sy=Ly, sz=Lz)
end function

function get_angularmomentum(id, Lx,Ly,Lz) result(ret)
  use amuse_mercuryMod
  integer :: ret, id
  real*8 :: Lx,Ly,Lz
  ret=get_particle_state(id, sx=Lx, sy=Ly, sz=Lz)
end function

function get_kinetic_energy(ek) result(ret)
  use amuse_mercuryMod
  integer :: ret
  real*8 :: ek
  call energy_angular_momentum(0,ek=ek)
  ret=0
end function

function get_potential_energy(ep) result(ret)
  use amuse_mercuryMod
  integer :: ret
  real*8 :: ep
  call energy_angular_momentum(0,ep=ep)
  ret=0
end function

function get_total_energy(e_tot) result(ret)
  use amuse_mercuryMod
  integer :: ret
  real*8 :: e_tot
  call total_energy_angular_momentum(e_tot=e_tot)
  ret=0
end function

function get_total_mass(m_tot) result(ret)
  use amuse_mercuryMod
  integer :: ret
  real*8 :: m_tot
  m_tot = 1
  ret=0
end function

function get_total_angular_momentum(am_tot) result(ret)
  use amuse_mercuryMod
  integer :: ret
  real*8 :: am_tot
  call total_energy_angular_momentum(am_tot=am_tot)
  ret=0
end function

function get_energy_deviation(delta_e) result(ret)
  use amuse_mercuryMod
  integer :: ret
  real*8 :: delta_e
  call energy_angular_momentum_deviation(delta_e=delta_e)
  ret=0
end function
  
function get_number_of_orbiters(norbiters) result(ret)
  use amuse_mercuryMod
  integer :: ret,norbiters
  ret=get_number_of_particles(norbiters)
end function  

function get_begin_time(system_time) result(ret)
      use amuse_mercuryMod
      implicit none
      integer :: ret
      real*8, intent(out) :: system_time

      ret = mercury_get_begin_time(system_time)
end function  

function set_begin_time(system_time) result(ret)
      use amuse_mercuryMod
      implicit none
      integer :: ret
      real*8, intent(in) :: system_time

      ret = mercury_set_begin_time(system_time)
end function  

function get_index_of_first_particle(index_of_the_particle) result(ret)
      use amuse_mercuryMod
      implicit none
      integer :: ret
      integer, intent(out) :: index_of_the_particle
      index_of_the_particle = 1

      ret = 0
end function

function get_index_of_next_particle(index_of_the_particle, &
    index_of_the_next_particle) result(ret)
      use amuse_mercuryMod
      implicit none
      integer :: ret
      integer, intent(in) :: index_of_the_particle
      integer, intent(out) :: index_of_the_next_particle
      index_of_the_next_particle = index_of_the_particle + 1

      ret = 0
end function

function get_total_radius(radius) result(ret)
      use amuse_mercuryMod
      implicit none
      integer :: ret
      real*8, intent(out) :: radius
      radius = 0

      ret = -1
end function

function new_particle(index_of_the_particle, mass, x, y, z, vx, vy, vz,  &
    radius) result(ret)
      use amuse_mercuryMod
      implicit none
      integer :: ret
      integer, intent(out) :: index_of_the_particle
      real*8, intent(in) :: mass
      real*8, intent(in) :: x
      real*8, intent(in) :: y
      real*8, intent(in) :: z
      real*8, intent(in) :: vx
      real*8, intent(in) :: vy
      real*8, intent(in) :: vz
      real*8, intent(in) :: radius

      ret = 0
end function

function get_state(index_of_the_particle, mass, x, y, z, vx, vy, vz,  &
    radius) result(ret)
      use amuse_mercuryMod
      implicit none
      integer :: ret
      integer, intent(in) :: index_of_the_particle
      real*8, intent(out) :: mass
      real*8, intent(out) :: x
      real*8, intent(out) :: y
      real*8, intent(out) :: z
      real*8, intent(out) :: vx
      real*8, intent(out) :: vy
      real*8, intent(out) :: vz
      real*8, intent(out) :: radius

      ret = 0
end function

function set_state(index_of_the_particle, mass, x, y, z, vx, vy, vz,  &
    radius) result(ret)
      use amuse_mercuryMod
      implicit none
      integer :: ret
      integer, intent(in) :: index_of_the_particle
      real*8, intent(in) :: mass
      real*8, intent(in) :: x
      real*8, intent(in) :: y
      real*8, intent(in) :: z
      real*8, intent(in) :: vx
      real*8, intent(in) :: vy
      real*8, intent(in) :: vz
      real*8, intent(in) :: radius

      ret = 0
end function

function set_eps2(epsilon_squared) result(ret)
      use amuse_mercuryMod
      implicit none
      integer :: ret
      real*8, intent(in) :: epsilon_squared

      ret = 0
end function

function get_eps2(epsilon_squared) result(ret)
      use amuse_mercuryMod
      implicit none
      integer :: ret
      real*8, intent(out) :: epsilon_squared
      epsilon_squared = 0

      ret = 0
end function

function get_potential(index_of_the_particle, potential) result(ret)
      use amuse_mercuryMod
      implicit none
      integer :: ret
      integer, intent(in) :: index_of_the_particle
      real*8, intent(out) :: potential
      potential = 0

      ret = 0
end function


function get_number_of_particles(number_of_particles) result(ret)
      use amuse_mercuryMod
      implicit none
      integer :: ret
      integer, intent(out) :: number_of_particles
      number_of_particles = 0

      ret = 0
end function

function set_acceleration(index_of_the_particle, ax, ay, az) result(ret)
      use amuse_mercuryMod
      implicit none
      integer :: ret
      integer, intent(in) :: index_of_the_particle
      real*8, intent(in) :: ax
      real*8, intent(in) :: ay
      real*8, intent(in) :: az

      ret = 0
end function

function get_acceleration(index_of_the_particle, ax, ay, az) result(ret)
      use amuse_mercuryMod
      implicit none
      integer :: ret
      integer, intent(in) :: index_of_the_particle
      real*8, intent(out) :: ax
      real*8, intent(out) :: ay
      real*8, intent(out) :: az
      ax = 0
      ay = 0
      az = 0

      ret = 0
end function

function get_center_of_mass_position(x, y, z) result(ret)
      use amuse_mercuryMod
      implicit none
      integer :: ret
      real*8, intent(out) :: x
      real*8, intent(out) :: y
      real*8, intent(out) :: z
      x = 0
      y = 0
      z = 0

      ret = 0
end function

function get_center_of_mass_velocity(vx, vy, vz) result(ret)
      use amuse_mercuryMod
      implicit none
      integer :: ret
      real*8, intent(out) :: vx
      real*8, intent(out) :: vy
      real*8, intent(out) :: vz
      vx = 0
      vy = 0
      vz = 0

      ret = 0
end function

function get_time_step(time_step) result(ret)
      use amuse_mercuryMod
      implicit none
      integer :: ret
      real*8, intent(out) :: time_step
      time_step = 0

      ret = -1
end function

function get_radius(id, radius) result(ret)
      use amuse_mercuryMod
      implicit none
      integer :: ret
      integer, intent(in) :: id
      real*8, intent(out) :: radius
      radius = 0

      ret = 0
end function

function set_radius(id, radius) result(ret)
      use amuse_mercuryMod
      implicit none
      integer :: ret
      integer, intent(in) :: id
      real*8, intent(in) :: radius

      ret = 0
end function


function get_potential_at_point(eps1, x1, y1, z1, phi, &
          number_of_points) result(ret)
      use amuse_mercuryMod
      implicit none
      integer :: ret
      real*8, intent(in) :: eps1
      real*8, intent(in) :: x1
      real*8, intent(in) :: y1
      real*8, intent(in) :: z1
      real*8, intent(out) :: phi
      integer, intent(in) :: number_of_points
      phi = 0

      ret = 0
end function

function get_gravity_at_point(eps1, x1, y1, z1, fx, fy, fz, &
           number_of_points) result(ret)
      use amuse_mercuryMod
      implicit none
      integer :: ret
      real*8, intent(in) :: eps1
      real*8, intent(in) :: x1
      real*8, intent(in) :: y1
      real*8, intent(in) :: z1
      real*8, intent(out) :: fx
      real*8, intent(out) :: fy
      real*8, intent(out) :: fz
      integer, intent(in) :: number_of_points
      fx = 0
      fy = 0
      fz = 0

      ret = 0
end function

end module
