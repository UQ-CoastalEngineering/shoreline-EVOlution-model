clear all; close all

case_name='example';

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% All Possible Inputs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
variable{01}='grid';
% icond %% chainage, x0, z0, x1, z1, dx ---%----%----%----%----%----%----%----%----%----%----%----%----%----%----%----%
variable{02}='icond';
% groynes %% chainage, groyne_x, start_time, end_time, comment %----%----%----%----%----%----%----%----%----%----%----%
variable{03}='groyne';
% seawall %% chainage, wall_x, wall_z %----%----%----%----%----%----%----%----%----%----%----%----%----%----%----%----%
variable{04}='seawall';
% xpar %% chainage, z_dune, s_dune, a, s_trans, s_off, z_off, ke, ka, hb_min, description ---%----%----%----%----%----%
variable{05}='xpar';
% waves %% time, wvht, wvper, wvdir --%----%----%----%----%----%----%----%----%----%----%----%----%----%----%----%----%
variable{06}='wave';
% wl %% time, wl -%----%----%----%----%----%----%----%----%----%----%----%----%----%----%----%----%----%----%----%----%
variable{07}='wl';
% transport %% time, transport --%----%----%----%----%----%----%----%----%----%----%----%----%----%----%----%----%----%
variable{08}='transport';
% xsect_ids %% chainage, id %----%----%----%----%----%----%----%----%----%----%----%----%----%----%----%----%----%----%
variable{09}='xsect_ids';
% xsect_ids_eq %%  chainage, id -%----%----%----%----%----%----%----%----%----%----%----%----%----%----%----%----%----%
variable{10}='xsect_ids_eq';

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Define Custom Inputs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 01) GRID FILE*************************************************************
grid.chainage(1)=0;
grid.base_x(1)=0;
grid.base_y(1)=0;
% cross shore distance
width=2000;
% along shore distance (total chainage)
len=4000;
% shoreline orientation ()
angle=0;
% grid transects (ncells + 1)
faces=3;
% cell size
spacing=len/(faces-1);
% complete grid
for i=1:faces-1
grid.base_x(i+1)=grid.base_x(i)+cosd(angle+90)*spacing;
grid.base_y(i+1)=grid.base_y(i)+sind(angle+90)*spacing;
grid.chainage(i+1)=grid.chainage(i)+sqrt([grid.base_x(i+1)-grid.base_x(i)]^2+[grid.base_y(i+1)-grid.base_y(i)]^2);
end
grid.end_x=grid.base_x+cosd(angle)*width;
grid.end_y=grid.base_y+sind(angle)*width;

plot(grid.base_x,grid.base_y,'g^','markersize',8); hold on
plot(grid.end_x,grid.end_y,'rs','markersize',8)
plot([grid.base_x;grid.end_x],[grid.base_y;grid.end_y],'b')


% 02) INITIAL CONDITION FILE************************************************
icond.chainage=[grid.chainage(1:end-1)+grid.chainage(2:end)]/2;
icond.x0=500*ones(size(icond.chainage));
icond.z0=ones(size(icond.chainage))*+0;
icond.x1=600*ones(size(icond.chainage));;
icond.z1=ones(size(icond.chainage))*-2;

% 03) GROYNE    ************************************************************

% 04) SEAWALL    ***********************************************************
seawall.chainage=[grid.chainage(1) grid.chainage(end)];
seawall.wall_x=[0 0];
seawall.wall_z=[0 0];
% 05) CROSS-SECTION PARAMETER FILE *****************************************

xpar.chainage=icond.chainage;
xpar.z_dune=4*ones(size(xpar.chainage));
xpar.s_dune=.1*ones(size(xpar.chainage));%*
%xpar.A=5./((icond.x1-icond.x0).^(2/3));
xpar.a=.15*ones(size(xpar.chainage));
xpar.s_trans=.05*ones(size(xpar.chainage));%*
xpar.s_off=.01*ones(size(xpar.chainage));
xpar.z_off=-20*ones(size(xpar.chainage));
xpar.ke=0.0000045;
xpar.ka=0.00000001;
xpar.hbmin=.6;%*
% 06) WAVES FILE ***********************************************************

%hs=1.5

wave.time(1)=datenum(2000,1,1,0,0,0);
wave.hsig(1)=.7;
wave.time(2)=datenum(2029,12,31,23,0,0);
wave.hsig(2)=.7;
gap=321;
wave.time(3)=datenum(2030,1,1,0,0,0);
wave.hsig(3)=3;
wave.time(4)=datenum(2030,1,1,24,0,0);
wave.hsig(4)=3;
wave.time(5)=datenum(2030,1,2,1,0,0);
wave.hsig(5)=.7;
wave.time(6)=datenum(2030,1,gap,23,0,0);
wave.hsig(6)=.7;

for qq=1:ceil(30*365/gap)
wave.time=[wave.time wave.time(3:6)+datenum(0,0,qq*gap,0,0,0)];
wave.hsig=[wave.hsig wave.hsig(3:6)];
end

wave.tp=ones(size(wave.time))*10;
wave.wdir=ones(size(wave.time))*90;

% 07) TIDES FILE ***********************************************************
wl.time(1)=datenum(2000,1,1,0,0,0);
wl.WL(1)=0;
wl.time(1)=datenum(2000,1,1,0,0,0);
wl.WL(1)=0;

% t_time=datenum(2030,1,1,0,0,0):datenum(0,0,0,1,0,0):datenum(2060,1,1,0,0,0);
% wl.time=[wl.time(1) t_time];
% wl.WL=[wl.WL 2*sin(2*pi/(12.5/24)*(t_time-t_time(1)))];




% 08) TRANSPORT FILE *******************************************************
transport.time(1)=datenum(2000,1,1);
transport.time(2)=datenum(2100,1,1);
transport.Q(1)=0;
transport.Q(2)=0;

% 09) XSECTION FILE *******************************************************
xsect_ids.chainage(1)=1000;
xsect_ids.name=2;

% 10) EQ XSECTION FILE ****************************************************
xsect_ids_eq.chainage(1)=1000;
xsect_ids_eq.name=2;



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Prepare lookup table
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Dir ---------------------------------
Direction{1}=[0;180];

% Per ---------------------------------
Period{1}=[0;20];

% Ht ---------------------------------
Height{1}=[0;10];

% Ny ---------------------------------
Chainage{1}=[500;1500];
Depth{1}=[20;20];
Contour_Rotation{1}=[0;0];

Ndir=length(Direction{1});
Nper=length(Period{1});
NHt=length(Height{1});
Ny=length(Chainage{1});

D{1}=ones(Ndir,Nper,NHt,Ny)*0;
K{1}=ones(Ndir,Nper,NHt,Ny)*1;

% -------------------------------------

dumb.lookup.Direction{1}=Direction{1};
dumb.lookup.Period{1}=Period{1};
dumb.lookup.Height{1}=Height{1};
dumb.lookup.Chainage{1}=Chainage{1};
dumb.lookup.Depth{1}=Depth{1};
dumb.lookup.Contour_Rotation{1}=Contour_Rotation{1};
dumb.lookup.Ndir=Ndir;
dumb.lookup.Nper=Nper;
dumb.lookup.NHt=NHt;
dumb.lookup.Ny=Ny;
dumb.lookup.D{1}=D{1};
dumb.lookup.K{1}=K{1};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Save Custom Inputs & lookup table
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
for i=1:length(variable);
    if exist(variable{i})>0;
        statement=['dumb.',variable{i},'=',variable{i},';'];
        eval(statement)
    end
end

save(['./../matfiles/',case_name,'.mat'], '-struct', 'dumb');
