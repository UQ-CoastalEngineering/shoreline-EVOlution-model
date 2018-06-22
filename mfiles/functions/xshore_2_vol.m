function [volume]=xshore_2_vol(xshore,in)

% Find dimensions of the output.
nt=length(xshore.time)
ny=length(xshore.chainage)

% Cross-sshore parameters
% all slopes are listed as positive.
b0=xshore.Z0+(ones(nt,1)*in.xpar.s_dune').*xshore.X0;
x_dune=((ones(nt,1)*in.xpar.z_dune')-b0)./-(ones(nt,1)*in.xpar.s_dune');
a=(xshore.Z0-xshore.Z1)./(xshore.X1-xshore.X0).^(2/3);
b1=xshore.Z1+(ones(nt,1)*-in.xpar.s_transition').*-xshore.X1;
b2=ones(nt,1)*(in.xpar.z_off+(-in.xpar.s_off.*-in.xpar.m))';
x_transition=-(b2-b1)./(ones(nt,1)*(in.xpar.s_transition-in.xpar.s_off)');
z_transition=(ones(nt,1)*-in.xpar.s_off').*x_transition+b2;
z_dune=ones(nt,1)*in.xpar.z_dune';
z_off=ones(nt,1)*in.xpar.z_off';
x_off=ones(nt,1)*in.xpar.m';
x_seawall=ones(nt,1)*in.seawall.x';
z_seawall=ones(nt,1)*in.seawall.z';
x_msl=(xshore.Z0./a).^(3/2)+xshore.X0;
z_min_dune_seawall=min(z_dune,z_seawall);


% sub aerial 
a1=[z_dune.*x_dune];
a2=[(xshore.X0-x_dune).*(0.5*(z_dune+xshore.Z0))];
a3=[(x_msl-xshore.X0).*xshore.Z0];
a4=[(a.*((x_msl-xshore.X0).^(5/3)))./(5/3)];

% sub aqueous
a5=[-z_off.*xshore.X1];
a6=[(a.*((xshore.X1-xshore.X0).^(5/3)))./(5/3)];
a7=[(xshore.Z0.*(xshore.X1-x_msl))];
a8=[(-z_off+0.5*(xshore.Z1+z_transition)).*(x_transition-xshore.X1)];
a9=[0.5*(-z_off+z_transition).*(x_off-x_transition)];


s1=zeros(size(a1));% sub-aerial
s2=zeros(size(a1));% sub-aqueous
s3=zeros(size(a1));% above dune

x_top=(z_seawall-b0)./(ones(nt,1)*-in.xpar.s_dune'); % Profile (Dune slope) intersection with the top of seawall.
z_toe=(ones(nt,1)*-in.xpar.s_dune').*(x_seawall)+b0;
z_toet=(ones(nt,1)*-in.xpar.s_transition').*(x_seawall)+b1;
x_max_dune_top=max(x_dune,x_top);

%
ii=( x_seawall >= x_dune & x_seawall <= xshore.X0 & x_seawall >= x_top);
s1(ii)=[x_seawall(ii)-x_max_dune_top(ii)]/2.*[z_min_dune_seawall(ii)-z_toe(ii)];

%
ii=( x_seawall >= xshore.X0 & x_seawall <= x_msl);
s1(ii)=[a(ii).*[x_seawall(ii)-xshore.X0(ii)].^(5/3)]/(5/3)+ ...
[x_seawall(ii)-(x_max_dune_top(ii)+xshore.X0(ii))/2].*[z_min_dune_seawall(ii)-xshore.Z0(ii)];

%
ii=( x_seawall >= x_msl & x_seawall <= xshore.X1 );
s1(ii)=a4(ii)+[(x_seawall(ii)-x_msl(ii)).*xshore.Z0(ii)]+ ...
[x_seawall(ii)-(x_max_dune_top(ii)+xshore.X0(ii))/2].*[z_min_dune_seawall(ii)-xshore.Z0(ii)];
s2(ii)=[a(ii).*[x_seawall(ii)-xshore.X0(ii)].^(5/3)]/(5/3)-a4(ii)-[(x_seawall(ii)-x_msl(ii)).*xshore.Z0(ii)];

%
ii=( x_seawall > xshore.X1 & x_seawall < x_transition);
s1(ii)=a4(ii)+[(xshore.X1(ii)-x_msl(ii)).*xshore.Z0(ii)]+ ...
[xshore.X1(ii)-(x_max_dune_top(ii)+xshore.X0(ii))/2].*[z_min_dune_seawall(ii)-xshore.Z0(ii)]+ ...
[(x_seawall(ii)-xshore.X1(ii)).*z_min_dune_seawall(ii)];

s2(ii)=[a(ii).*[xshore.X1(ii)-xshore.X0(ii)].^(5/3)]/(5/3)-a4(ii)-[(xshore.X1(ii)-x_msl(ii)).*xshore.Z0(ii)]- ...
[(x_seawall(ii)-xshore.X1(ii)).*((xshore.Z1(ii)+z_toet(ii))/2)];

ii=( z_seawall >= z_dune );
s3(ii)=[x_seawall(ii).*[z_seawall(ii)-z_dune(ii)]];

% format bank
% disp('!!!!!!!!!!!!!!!!!!!!!')
% a1(1,25)
% a2(1,25)
% a3(1,25)
% a4(1,25)
% disp('!!!!!!!!!!!!!!!!!!!!!')
% x_msl(1,25)
% s1(1,25)
% s2(1,25)
% s3(1,25)
% disp('!!!!!!!!!!!!!!!!!!!!!')
% a(1,25)*100

volume.subaerial=a1+a2+a3-a4+s1+s3;
volume.subaqueous=a5-a6+a4+a7+a8+a9+s2;
volume.total=volume.subaerial+volume.subaqueous;
volume.x_msl=x_msl;
volume.z_toet=z_toet;

end

