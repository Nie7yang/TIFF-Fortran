    !*****************************************************************
    !************************** Tiff module **************************
    !  Nie qiyang 2022.12.13
    !  ������ �Ϻ���������о�Ժ
    !  ����tiff��ʽ��׼��ȡ��������������ֵ����ʱδ֧�ִַ��洢�����ݸ�ʽ������Ҫ��չ�����DN==273����ר��ִ���ȡ��

    module Tiff
    Use, Intrinsic :: ISO_C_BINDING
    Implicit None
    save
    character(len=2) :: IFH_PaiXu
    Integer(kind=C_SHORT) :: IFH_V,IFD_DE_N  !C_SHORTռ2�ֽ�
    Integer(kind=C_SHORT) , allocatable:: DE_tag2(:)
    Integer , allocatable:: DE_tag4(:)
    Integer(kind=C_LONG) ::DiZhi
    Integer::IFH_D
    Integer::DN,hang,lie,tou,nodateN
    Real , allocatable :: dat(:,:) !// �������
    Character(len=:) , allocatable::nodatecha,nodatechb
    !character(len=7)::nodatecha
    real::nodate

    contains !ģ�����

    subroutine Tiff_read(FileName)
    integer::i
    Character(len=:) , allocatable ,intent(in):: FileName
    Open( 12 , File = FileName , access="stream" , form = "unformatted" )
    !ͼ���ļ�ͷIFH
    Read( 12) IFH_PaiXu  !1-2�ֽ�,II��ʮ������49 49����ʾIntel��ʽ �����ֽ���ǰ�����ֽ��ں�;MM��ʮ������4D 4D����ʾMotorala��ʽ�����ֽ���ǰ�����ֽ��ں�
    !write(*,*)IFH_PaiXu
    Read( 12 ) IFH_V   !3-4�ֽ�,����汾�ţ�42=��ͨ�汾��43=big TIFF�汾��
    !write(*,*)IFH_V
    Read( 12 )IFH_D    !5-8�ֽ�,ָ���ļ��ĵ�һ��ͼ���ļ�Ŀ¼�������ֽ�λ�á����Ϊ8��������ļ���
    !write(*,*)IFH_D
    !һ��TIFF�ļ���������1��IFD,һ��IFD��������һ��DE��DE�ĸ����ǲ����ģ���Ϊÿ��DEֻ��ʶ��ͼ���һ������.
    Read( 12 )IFD_DE_N   !2���ֽڣ�������IFD��DE(Directory Entry,Ŀ¼��)��������
    !write(*,*)IFD_DE_N

    allocate(DE_tag2(IFD_DE_N),DE_tag4(IFD_DE_N))
    do i=1,IFD_DE_N
        write(*,*)'Tag',i,':'
        Read(12)DE_tag2(i)    !2���ֽڣ���ǩ���  demһ���һ���ǿ�
        DN=DE_tag2(i)
        write(*,*)DE_tag2(i)
        Read(12)DE_tag2(i)    !2���ֽڣ���������
        write(*,*)DE_tag2(i)
        Read(12)DE_tag4(i)    !4���ֽڣ����������ݵ�����
        write(*,*)DE_tag4(i)
        Read(12)DE_tag4(i)    !4���ֽڣ����ռ�õ��ֽ�������4�� ������ֱ�Ӵ��ڴˡ� �������4�����������ŵ���ָ��ʵ�����ݵ�ָ�롣DEMһ����ǰ�ߡ�
        if(DN==256)then
            lie=DE_tag4(i)
        elseif(DN==257)then
            hang=DE_tag4(i)
        elseif(DN==273)then
            tou=DE_tag4(i)
        elseif(DN== -23423)then  !tiff�����������Ϊ�޷��Ŷ����Σ�fortran�����з��ű��������Ϊ����
            nodateN=DE_tag4(i)
        endif
        write(*,*)DE_tag4(i)
    enddo
    !write(*,*)hang,lie,tou
    Read( 12,Pos=tou+1 )Dizhi
    tou=Dizhi+1   !��ȡ�����ݵľ����ַ

    nodatecha='          '
    nodatechb='          '
    Read( 12,Pos=nodateN+1 )nodatechb  !����nodata��ַ����nodataֵ
    Read(nodatechb,*)nodatecha
    nodatecha=trim(nodatecha)
    Read(nodatecha,*)nodate
    write(*,*)nodate

    allocate(dat(lie,hang))

    Read( 12,Pos=tou )dat
    close(12)
    return
    end subroutine Tiff_read

    end module Tiff
    !************************** Tiff module **************************
    !*****************************************************************




    program name
    Use Tiff
    Use, Intrinsic :: ISO_C_BINDING
    Implicit None

    Real::k
    Character(len=:) , allocatable:: fileOld,fileNew
    !Open( 12 , File = "C:\Users\nieqi\Desktop\L\10shihou_cai.tif" , access="stream" , form = "unformatted" )

    integer::Zhuangtai,kk,i
    integer,external::SYSTEM
    fileOld="C:\Users\nieqi\Desktop\L\10shihou_cai.tif"

    fileNew=fileOld(1:len(fileOld)-4)//'_change.tif'
    write(*,*)fileNew

    kk=SYSTEM('copy '//fileOld//' '//fileNew)

    call Tiff_read(fileNew)  !����Tiff��ȡ��
    write(*,*)dat(1,1)

    dat=1.0
    Open( 12 , File = fileNew , access="stream" , form = "unformatted" )
    !write(*,*)dat(6,1)
    write(12,Pos=tou)dat
    close(12)
    deallocate(DE_tag2,DE_tag4,dat)
    end program name