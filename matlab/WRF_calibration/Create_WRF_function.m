function [structure] = Create_WRF_function(structure,data)

last=size(structure.Date,1);

for i=1:size(data.PH,2)
    structure.Data.PH(last,:,i)=data.PH(:,i)';
end
for i=1:size(data.QV,2)
    structure.Data.QV(last,:,i)=data.QV(:,i)';
end
for i=1:size(data.TH,2)
    structure.Data.TH(last,:,i)=data.TH(:,i)';
end
for i=1:size(data.TS,2)
    structure.Data.TS(last,:,i)=data.TS(:,i)';
end
for i=1:size(data.UU,2)
    structure.Data.UU(last,:,i)=data.UU(:,i)';
end
for i=1:size(data.VV,2)
    structure.Data.VV(last,:,i)=data.VV(:,i)';
end

return