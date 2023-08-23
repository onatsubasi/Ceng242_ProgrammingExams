#include "jazz.h"
#include "metal.h"
#include "kpop.h"
#include "rock.h"


int MetalBand::play(MusicBand *other)
{
    if (get_energy() >= 0){
        int score;
        double c, k = 0.16;
        if (MusicBand* test = dynamic_cast<KPopBand*>(other))
            c = 0.5;
        else if (MusicBand* test = dynamic_cast<MetalBand*>(other))
            c = 1.0;
        else if (MusicBand* test = dynamic_cast<RockBand*>(other))
            c = 1.5;
        else
            c = 1.1;

        score = (get_fan_count() + 0.1 * get_talent() * get_energy()) * c;
        set_energy( get_energy() - k*get_energy());
        return score;
    }
}

void MetalBand::rehearse(void) 
{
    if (get_energy() >= 0){
        double k = 0.16;
        int t = -5;
        set_energy( get_energy() - k*0.5*get_energy());
        set_talent(get_talent() + t);
    }
}