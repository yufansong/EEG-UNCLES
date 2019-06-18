function [ ] = Dat2matlab( person_number,chanlocs )
%DAT2MATLAB 此处显示有关此函数的摘要
%   此处显示详细说明
for index=1:6
I=[]
filename={'_1_all','_1_1','_1_2','_2_all','_2_1','_2_2'};
str=['onedata_deal_',num2str(person_number),filename{index},'.csv'];
M = csvread(str,1,1);
R=double(M');
n=size(R,2);

for i=1:n
scrsz = get(0,'ScreenSize');
set(gcf,'position',scrsz)
topoplot(R(:,i),chanlocs,'style','map','electrodes','labelpoint','chaninfo',chanlocs);
title(['cluster ' num2str(i)]);
saveas(gcf,[num2str(person_number),filename{index},'_cluster_' num2str(i),'.jpg']);
I=[I imcrop(imread([num2str(person_number),filename{index},'_cluster_' num2str(i),'.jpg']),[700,50,1100,1100])];
end

for i=1:n
for j=1:128
R(j,i)=R(j,i)*4*i;
end
end
figure
for i=1:128
R(i,n+1)=sum(R(i,1:n));
end
scrsz = get(0,'ScreenSize');
set(gcf,'position',scrsz)
topoplot(R(:,n+1),chanlocs,'style','both','electrodes','labelpoint','chaninfo',chanlocs);
title(['allcluster']);
saveas(gcf,[num2str(person_number),filename{index},'_allcluster','.jpg']);
I=[imcrop(imread([num2str(person_number),filename{index},'_allcluster','.jpg']),[700,50,1100,1100]) I];
imshow(I);
saveas(gcf,[num2str(person_number),filename{index},'_merge','.jpg']);
end
end
