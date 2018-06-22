function out = evo_rd_output(resfil,varargin)

out = struct();

fid = fopen(resfil,'r');
line = fgetl(fid);
i = 0;
while ~isempty(line)
    i = i + 1;
    [header{i}, line] = strtok(line,',');
end
Nhead = length(header);

if strcmp(header{1},'VARIABLE')
    typ = 1; % general type
    Ndat = Nhead - 2;
    format = ['%s %s',repmat(' %f',[1,Ndat'])];
elseif (strcmp(header{1},'XSECT_ID'))
    typ = 2; % xsect type
    Ndat = Nhead - 3;
    format = ['%s %s %s',repmat(' %f',[1,Ndat'])];
end

C = textscan(fid,format,'Delimiter',',');
if typ == 1
    Nvar = 1;
    varname{1} = '';
    while true
        tmpname = C{1}{Nvar};
        if ~strcmp(tmpname,varname{1});
            varname{Nvar} = tmpname;
            Nvar = Nvar + 1;
        else
            Nvar = Nvar - 1;
            break
        end
    end
    if strcmp(header{2},'TIME/CHAINAGE')
        out.time = datenum(C{2}(1:Nvar:end),'dd/mm/yyyy HH:MM:SS');
        NT = length(out.time);
        out.chainage = str2num(char(header(1,3:end)));
    end
    dat = cell2mat(C(3:end));
    for n = 1 : Nvar
        out.(varname{n}) = dat(n:Nvar:end,:);
    end
elseif typ==2
    Nvar = 1;
    varname{1} = '';
    while true
        tmpname = C{2}{Nvar};
        if ~strcmp(tmpname,varname{1}) % if not ture, keep cycling through the all variables
            varname{Nvar} = tmpname;
            Nvar = Nvar + 1;
        else
            Nvar = Nvar - 1;
            break
        end
    end
    i = 1;
    Nxs = 1;
    xsname{1} = '';
    while true
        tmpname = C{1}{i};
        if ~strcmp(tmpname,xsname{1});
            xsname{Nxs} = tmpname;
            Nxs = Nxs + 1;
            i = i + Nvar;
        else
            Nxs = Nxs - 1;
            break
        end
    end
    if strcmp(header{3},'TIME/POINT')
        out.time = datenum(C{3}(1:(Nxs*Nvar):end),'dd/mm/yyyy HH:MM');
        NT = length(out.time);
    end
    dat = cell2mat(C(4:end));
    n = 1;
    for i = 1 : Nxs
        for j = 1 : Nvar
            out.(xsname{i}).(varname{j}) = dat(n:(Nxs*Nvar):end,:);
            n = n + 1;
        end
    end
else
    error('This evo file type is not currently supported')
end


