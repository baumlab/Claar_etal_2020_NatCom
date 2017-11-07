% Produce a map of the 2015-2016 El Nino
%
% 

% Set patch that contains dhw_1516.mat
dhwmaxpath='.\';

% Use pre-compiled DHW values from AVHRR.
% Load data file
load([dhwmaxpath 'dhw_1516.mat']);
dhwmax16=double(nanmax(dhw_1516,[],3));
clear dhw_1516;

% Custom color-map
cmap_enso=[...
    0.6       0.9       0.8;
    0.8       0.7       0.9
    0.6350    0.0780    0.1840;
    0.3010    0.7450    0.9330;
    0.4660    0.6740    0.1880;
    0.4940    0.1840    0.5560;
    0.9290    0.6940    0.1250;
    0.8500    0.3250    0.0980;
    0    0.4470    0.7410;];


%% Make and export the figure
% Open figure
figure(1) 

% Set coordinates from 0 to 360 rather than 180 to 180
dhwmax_enso16=cat(2,dhwmax16(:,721:end,:),dhwmax16(:,1:720,:));

% Initialize equal-area projection and coordinates
m_proj('Hobo-Dyer','lat',[-25 25],'lon',[0 360]);
lon=0.125:0.25:360;
lat=-89.875:0.25:90;

m_contourf(lon,lat,dhwmax_enso16,50,'linestyle','none');
% m_hatch([360-145 360-145 360-117 360-117],[15 35 35 15],'single',62,4.5,'color','w','linewidth',1.5);

% Add coastline as patch and a grid
m_coast('patch',[.4 .4 .4]);
m_grid('fontsize',12);

% Place colorbar and label
cb=colorbar('EastOutside','fontsize',12);
xlabel(cb,['Maximum DHW'],'fontsize',12);
x1=get(gca,'position');
x=get(cb,'Position');
x(1)=0.797;
x(2)=0.33535;
x(3)=0.02;
x(4)=0.365;
set(cb,'Position',x)
set(gca,'position',x1)
% Label
set(gca,'clim',[0 35])

% Use a modified hot colormap
hrev=hot(30);
hrev=flipud(hrev);
colormap(hrev)
set(gcf, 'Position', [100, 100, 1000, 400]);

% Make the figure and export
drawnow;
% Define filename structure
fname=['figure_' num2str(get(gcf,'Number')) '.png'];
fname2=['figure_' num2str(get(gcf,'Number')) '.jpg'];
% Export figure
s=hgexport('readstyle','enso');
% This assumes the existence of a style "enso", which has to be pre-set in
% the export dialog for figures in MATLAB. Save this style as 'enso'. This
% has to be done on your computer.

% Set the export format
s.Format='png';
% Export saves it.
hgexport(gcf,fname,s);

% Set the export format
s.Format='jpg';
% Export saves it.
hgexport(gcf,fname2,s);
