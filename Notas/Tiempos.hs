module Notas.Tiempos where

import Notas.Musica as Musica

nc, nr, nb, nn, co, sc, fu, sf, nrp, nbp, nnp, ncp, 
    nsp, nfp, nndp, ncdp, nsdp :: Dur

ncs, nrs, nbs, nns, crs, scs, fus, sfs, nrps, nbps,
    nnps, ncps, nsps, nfps, nndps, ncdps, nsdps :: Musica Tono

nc    = 2;     ncs    = silen nc    -- nota cuadrada
nr    = 1;     nrs    = silen nr    -- nota redonda
nb    = 1/2;   nbs    = silen nb    -- nota blanca
nn    = 1/4;   nns    = silen nn    -- nota negra
co    = 1/8;   crs    = silen co    -- nota corchea
sc    = 1/16;  scs    = silen sc    -- nota semicorchea
fu    = 1/32;  fus    = silen fu    -- nota fusa
sf    = 1/64;  sfs    = silen sf    -- nota semifusa

nrp   = 3/2;   nrps   = silen nrp   -- duracion nota redonda con puntillo y su respectivo silencio
nbp   = 3/4;   nbps   = silen nbp   -- nota blanca con puntillo y silencio
nnp   = 3/8;   nnps   = silen nnp   -- nota negra con puntillo y silencio
ncp   = 3/16;  ncps   = silen ncp   -- nota corchea con puntillo y silencio
nsp   = 3/32;  nsps   = silen nsp   -- nota semicorchea con puntillo y silencio
nfp   = 3/64;  nfps   = silen nfp   -- nota fusa con puntillo y silencio

nndp  = 7/8;   nndps  = silen nndp  -- nota negra con doble puntillo y silencio
ncdp  = 7/16;  ncdps  = silen ncdp  -- nota corchea con doble puntillo y silencio
nsdp  = 7/32;  nsdps  = silen nsdp  -- nota semicorchea con doble puntillo y silencio

